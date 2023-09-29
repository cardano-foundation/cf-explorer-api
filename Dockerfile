FROM openjdk:18-jdk-slim AS build
RUN apt update -qq && apt install -y gettext-base
ARG PRIVATE_MVN_REGISTRY_URL
ARG PRIVATE_MVN_REGISTRY_USER
ARG PRIVATE_MVN_REGISTRY_PASS
ARG SETTINGS_XML_TPL=.m2/settings.default.xml.tpl
WORKDIR /app
COPY ${SETTINGS_XML_TPL} /root/${SETTINGS_XML_TPL}
RUN envsubst </root/${SETTINGS_XML_TPL} >/root/.m2/settings.xml
COPY pom.xml /app/pom.xml
COPY mvnw /app/mvnw
COPY .mvn /app/.mvn
RUN ./mvnw verify clean --fail-never
COPY . /app
RUN ./mvnw clean package -DskipTests

FROM openjdk:18-jdk-slim AS runtime
WORKDIR /app
COPY --from=build /app/target/*.jar /app/app.jar
COPY src/main/resources/networks /app/genesis
COPY src/main/resources/key /app/key
COPY src/main/resources/permission /app/permission

ENTRYPOINT ["java", "-jar", "app.jar"]
