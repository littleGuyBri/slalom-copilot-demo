# FILEPATH: /Users/bguthrie/code/slalom-copilot-demo/test_demo.py
import unittest
from unittest.mock import patch
import weather  # assuming your original file is named demo.py

class TestGetWeather(unittest.TestCase):

    @patch('demo.requests.get')
    def test_get_weather_success(self, mock_get):
        mock_response = mock_get.return_value
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "weather": [{"description": "clear sky"}],
            "main": {"temp": 280, "humidity": 80}
        }

        result = weather.get_weather("Seattle")
        expected_result = "Weather in Seattle: clear sky, Temperature: 280K, Humidity: 80%"
        self.assertEqual(result, expected_result)

    @patch('demo.requests.get')
    def test_get_weather_failure(self, mock_get):
        mock_response = mock_get.return_value
        mock_response.status_code = 404
        mock_response.json.return_value = {}

        result = weather.get_weather("InvalidCity")
        expected_result = "Failed to retrieve weather information."
        self.assertEqual(result, expected_result)

    @patch('demo.requests.get')
    def test_get_weather_new_york(self, mock_get):
        mock_response = mock_get.return_value
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "weather": [{"description": "sunny"}],
            "main": {"temp": 290, "humidity": 70}
        }

        result = weather.get_weather("New York")
        expected_result = "Weather in New York: sunny, Temperature: 290K, Humidity: 70%"
        self.assertEqual(result, expected_result)

if __name__ == '__main__':
    unittest.main()
    
    