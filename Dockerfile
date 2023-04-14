FROM openjdk:11-jdk-slim
ENV TZ=Asia/Ho_Chi_Minh
ARG JAR_FILE=target/*.jar
COPY ${JAR_FILE} app.jar
ENTRYPOINT ["java","-jar","/app.jar"]
