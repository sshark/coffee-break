import akka.dispatch.ExecutionContexts;
import akka.dispatch.Futures;
import akka.dispatch.OnFailure;
import akka.japi.Pair;
import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.concurrent.Await;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.concurrent.duration.Duration;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static akka.dispatch.Futures.future;

/**
 * @author Lim, Teck Hooi
 */

public class TakingCoffeOrdersSuite {
    private static final Logger LOGGER = LoggerFactory.getLogger(TakingCoffeOrdersSuite.class);

    static private DataSource dataSource;

    private String[] customerNames = {
        "Andrew", "James", "Adam",
        "Kitty", "Alice", "Bob",
        "Kathy"
    };

    @SuppressWarnings({"unchecked"})
    private Pair<String, Double>[] drinks = new Pair[] {
        new Pair("Latte", 10.0), new Pair("Cuppocino", 7.0), new Pair("Mocha", 11.0),
        new Pair("Cola", 3.0), new Pair("Milk Shake", 6.0), new Pair("Lemon Juice", 5.0),
        new Pair("Apple Juice", 5.0), new Pair("Water", 1.0),
    };

    private Random randomizer = new Random();

    public String randomName() {
        if (randomizer == null) {
            randomizer = new Random();
        }
        return customerNames[randomizer.nextInt(customerNames.length - 1)];
    }

    public Pair<String, Double> randomDrink() {
        if (randomizer == null) {
            randomizer = new Random();
        }
        return drinks[randomizer.nextInt(drinks.length - 1)];
    }

    @BeforeClass
    static public void commonSetup() throws Exception {
        dataSource = new ComboPooledDataSource();
    }

    @Test
    public void testInsertCoffeeOrders() throws Exception {

        Connection conn = dataSource.getConnection();
        PreparedStatement stmt = conn.prepareStatement("insert into drink_order values(null, ?, ?, ?)");
        long startTime = System.currentTimeMillis();

        for (int i = 0; i < 1000; i++) {
            stmt.setString(1, randomName());
            Pair<String, Double> drink = randomDrink();
            stmt.setString(2, drink.first());
            stmt.setDouble(3, drink.second());
            stmt.execute();
        }
        stmt.close();
        LOGGER.debug("Insertion took " + (System.currentTimeMillis() - startTime) + "ms");

        conn.close();
    }

    @Test
    public void testFastInsertCoffeeOrders() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(18);
        ExecutionContext ec = ExecutionContexts.fromExecutorService(pool);
        List<Future<Boolean>> results = new ArrayList<>();

        final Connection conn = dataSource.getConnection();
        long startTime = System.currentTimeMillis();
        final PreparedStatement stmt = conn.prepareStatement("insert into fast_drink_order values(null, ?, ?, ?)");

        for (int i = 0; i < 1000; i++) {
            Future<Boolean> result = future(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    stmt.setString(1, randomName());
                    Pair<String, Double> drink = randomDrink();
                    stmt.setString(2, drink.first());
                    stmt.setDouble(3, drink.second());
                    boolean result = stmt.execute();
                    return result;
                }
            }, ec);
            result.onFailure(new OnFailure() {
                @Override
                public void onFailure(Throwable failure) throws Throwable {
                    LOGGER.error("Insertions failed.", failure);
                }
            }, ec);
            results.add(result);
        }

        Await.ready(Futures.sequence(results, ec), Duration.create(5, TimeUnit.MINUTES));
        LOGGER.debug("Insertion took " + (System.currentTimeMillis() - startTime) + "ms");

        pool.shutdown();
        stmt.close();
        conn.close();
    }
}
