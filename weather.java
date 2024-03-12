// FILEPATH: /Users/bguthrie/code/slalom-copilot-demo/WeatherLookup.java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import org.json.JSONObject;

public class WeatherLookup {
    private static final String API_KEY = "YOUR_API_KEY";  // Replace with your actual API key

    public static void main(String[] args) throws Exception {
        String city = "Seattle";
        getWeather(city);
    }

    public static void getWeather(String city) throws Exception {
        String url = "http://api.openweathermap.org/data/2.5/weather?q=" + city + "&appid=" + API_KEY;

        URL obj = new URL(url);
        HttpURLConnection con = (HttpURLConnection) obj.openConnection();
        con.setRequestMethod("GET");

        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuffer response = new StringBuffer();

        while ((inputLine = in.readLine()) != null) {
            response.append(inputLine);
        }
        in.close();

        JSONObject myResponse = new JSONObject(response.toString());

        String weather = myResponse.getJSONArray("weather").getJSONObject(0).getString("description");
        double temperature = myResponse.getJSONObject("main").getDouble("temp");
        int humidity = myResponse.getJSONObject("main").getInt("humidity");

        System.out.println("Weather in " + city + ": " + weather + ", Temperature: " + temperature + "K, Humidity: " + humidity + "%");
    }
}