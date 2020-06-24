FROM openjdk:11-jre-slim
ENTRYPOINT ["sudo", "mkdir", "/app/"]
WORKDIR app
COPY todo-app.jar /app
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "/app/todo-app.jar", "-Dhttp.host=0.0.0.0"]