import requests

def get_weather(city):
    api_key = "YOUR_API_KEY"  # Replace with your actual API key
    url = f"http://api.openweathermap.org/data/2.5/weather?q={city}&appid={api_key}"
    
    response = requests.get(url)
    data = response.json()
    
    if response.status_code == 200:
        weather = data["weather"][0]["description"]
        temperature = data["main"]["temp"]
        humidity = data["main"]["humidity"]
        
        return f"Weather in {city}: {weather}, Temperature: {temperature}K, Humidity: {humidity}%"
    else:
        return "Failed to retrieve weather information."

# Example usage
city = "Seattle"
print(get_weather(city))
