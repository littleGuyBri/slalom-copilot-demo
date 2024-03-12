provider "aws" {
  region = "us-west-2"  // Replace with your AWS region
}

data "aws_iam_policy_document" "lambda_assume_role_policy" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "lambda_execution_role" {
  name               = "lambda_execution_role"
  assume_role_policy = data.aws_iam_policy_document.lambda_assume_role_policy.json
}

resource "aws_lambda_function" "weather_lookup" {
  function_name = "WeatherLookup"
  handler       = "WeatherLookup::getWeather"
  role          = aws_iam_role.lambda_execution_role.arn
  runtime       = "java11"

  filename = "weatherlookup.jar"

  environment {
    variables = {
      API_KEY = "YOUR_API_KEY"  // Replace with your actual API key
    }
  }
}