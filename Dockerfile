FROM openjdk:17-jdk-slim AS build
RUN apt-get update && apt-get install -y fontconfig libfreetype6 && rm -rf /var/lib/apt/lists/*
ENV TZ=Asia/Ho_Chi_Minh
WORKDIR /app
COPY .m2/settings.xml /root/.m2/settings.xml
COPY pom.xml /app/pom.xml
COPY mvnw /app/mvnw
COPY .mvn /app/.mvn
RUN ./mvnw verify clean --fail-never
COPY . /app
RUN ./mvnw clean package -DskipTests

FROM openjdk:17-jdk-slim AS runtime
RUN apt-get update && apt-get install -y fontconfig libfreetype6 && rm -rf /var/lib/apt/lists/*
ENV TZ=Asia/Ho_Chi_Minh
COPY --from=build /app/target/*.jar /app/cardano-explorer-api.jar
WORKDIR /app
ENTRYPOINT ["java", "-jar", "cardano-explorer-api.jar"]
