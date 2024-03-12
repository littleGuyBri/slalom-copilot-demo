// FILEPATH: /Users/bguthrie/code/slalom-copilot-demo/WeatherLookupTest.java
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import static org.mockito.Mockito.*;

@RunWith(MockitoJUnitRunner.class)
public class WeatherLookupTest {

    @Mock
    HttpURLConnection mockConnection;

    @Test
    public void testGetWeatherSuccess() throws Exception {
        String json = "{\"weather\":[{\"description\":\"clear sky\"}],\"main\":{\"temp\":280,\"humidity\":80}}";
        InputStream inputStream = new ByteArrayInputStream(json.getBytes());

        when(mockConnection.getResponseCode()).thenReturn(200);
        when(mockConnection.getInputStream()).thenReturn(inputStream);

        WeatherLookup.getWeather("Seattle");

        verify(mockConnection, times(1)).getResponseCode();
        verify(mockConnection, times(1)).getInputStream();
    }

    @Test
    public void testGetWeatherFailure() throws Exception {
        when(mockConnection.getResponseCode()).thenReturn(404);

        WeatherLookup.getWeather("InvalidCity");

        verify(mockConnection, times(1)).getResponseCode();
    }
}
