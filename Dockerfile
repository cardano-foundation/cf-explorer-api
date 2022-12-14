FROM openjdk:11-jdk-slim AS build
COPY . /app
WORKDIR /app
COPY .m2/settings.xml /root/.m2/settings.xml
RUN ./mvnw clean package -DskipTests 


FROM openjdk:11-jdk-slim AS runtime
COPY --from=build /app/target/*.jar /app/app.jar
WORKDIR /app
ENTRYPOINT ["java", "-jar", "app.jar"]
