# Build Image
FROM openjdk:11-jdk-slim AS build
ENV TZ=Asia/Ho_Chi_Minh
WORKDIR /app
COPY .m2/settings.xml /root/.m2/settings.xml
COPY pom.xml /app/pom.xml
COPY mvnw  /app/mvnw
COPY .mvn /app/.mvn
RUN ./mvnw verify clean --fail-never
COPY . /app
RUN ./mvnw clean package -DskipTests

FROM openjdk:11-jdk-slim AS runtime
ENV TZ=Asia/Ho_Chi_Minh
COPY --from=build /app/target/*.jar /app/cardano-explorer-api.jar
WORKDIR /app
ENTRYPOINT ["java", "-jar", "cardano-explorer-api.jar"]
