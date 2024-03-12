## Running the application

To run this application, you need to replace "YOUR_API_KEY" with your actual OpenWeatherMap API key. You also need to have the org.json package in your classpath. You can get it from here.

To compile and run the application, you can use the following commands in your terminal:

Replace json-20210307.jar with the actual filename of the org.json JAR file you downloaded.
---
# Documentation for `WeatherLookup.java`

`WeatherLookup.java` is a Java application that uses the OpenWeatherMap API to fetch the current weather information for a specified city. 

## Class: `WeatherLookup`

This class contains the main method to run the application and a method to get the weather information.

### Method: `public static void main(String[] args)`

This is the entry point of the application. It calls the `getWeather` method with "Seattle" as the argument.

### Method: `public static void getWeather(String city)`

This method takes a city name as an argument and fetches the current weather information for that city.

#### Process:

1. Construct the URL for the API request using the city name and the API key.

2. Open a connection to the URL and set the request method to GET.

3. Read the response from the connection into a `StringBuffer`.

4. Convert the response to a `JSONObject`.

5. Extract the weather description, temperature, and humidity from the `JSONObject`.

6. Print the weather information to the console.

## Usage:

To run this application, replace `"YOUR_API_KEY"` with your actual OpenWeatherMap API key. Compile and run the application using the Java compiler (`javac`) and Java application launcher (`java`).
---
## Tests Documentation

# Documentation for `WeatherLookupTest.java`

`WeatherLookupTest.java` is a set of unit tests for the `WeatherLookup` class in the `WeatherLookup.java` file. It uses JUnit for the testing framework and Mockito for mocking objects.

## Class: `WeatherLookupTest`

This class contains the unit tests for the `WeatherLookup` class.

### Setup:

The `@Mock` annotation is used to create a mock object for `HttpURLConnection`. This mock object is used to simulate different responses from the OpenWeatherMap API.

### Test 1 - `testGetWeatherSuccess()`

This test checks the successful execution of the `getWeather` method. The mock object is set up to simulate a successful API response with a status code of 200 and a specific JSON payload. The `getWeather` method is then called with the argument "Seattle", and the interactions with the mock object are verified.

### Test 2 - `testGetWeatherFailure()`

This test checks the failure scenario of the `getWeather` method. The mock object is set up to simulate an unsuccessful API response with a status code of 404. The `getWeather` method is then called with the argument "InvalidCity", and the interactions with the mock object are verified.

## Usage:

To run these tests, you need to have JUnit and Mockito in your classpath. Compile and run the tests using the Java compiler (`javac`) and Java application launcher (`java`).
---
## Deployment Documentation

# Documentation for `deployment_java_aws.tf`

`deployment_java_aws.tf` is a Terraform configuration file that deploys the `WeatherLookup` Java application to an AWS Lambda function.

## Provider: `aws`

This block configures the AWS provider for Terraform. The `region` attribute should be replaced with your AWS region.

## Data Source: `aws_iam_policy_document`

This block defines an IAM policy that allows the Lambda function to assume a role. The policy allows the `sts:AssumeRole` action for the `lambda.amazonaws.com` service.

## Resource: `aws_iam_role`

This block creates an IAM role with the name `lambda_execution_role`. The role uses the policy defined in the `aws_iam_policy_document` data source.

## Resource: `aws_lambda_function`

This block creates a Lambda function with the following attributes:

- `function_name`: The name of the Lambda function, which is `WeatherLookup`.

- `handler`: The handler for the Lambda function, which is the `getWeather` method in the `WeatherLookup` class.

- `role`: The IAM role that the Lambda function assumes, which is the `lambda_execution_role`.

- `runtime`: The runtime for the Lambda function, which is `java11`.

- `filename`: The source code for the Lambda function, which is the `weatherlookup.jar` file.

- `environment`: The environment variables for the Lambda function. The `API_KEY` variable should be replaced with your actual OpenWeatherMap API key.

## Usage

To apply this configuration, you need to have the AWS CLI installed and configured with your AWS credentials. You also need to have Terraform installed. You can apply the configuration with the following commands:

```bash
terraform init
terraform apply