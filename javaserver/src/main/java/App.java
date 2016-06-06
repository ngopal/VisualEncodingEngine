import java.io.IOException;

import static spark.Spark.*;

/**
 * Created by nikhilgopal on 6/6/16.
 */
public class App {
    public static void main(String[] args) {
        get("/hello", (req, res) ->
                "Hello World");

    }
}
