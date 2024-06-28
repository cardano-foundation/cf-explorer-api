package org.cardanofoundation.explorer.api.config.aop.singletoncache;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.TimeUnit;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializer;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.CodeSignature;

@Aspect
@Component
@Log4j2
@RequiredArgsConstructor
public class SingletonCallAspect {

  @Value("${application.network}")
  private String network;

  private static final String LOCKED = "LOCKED";
  private static final String PREFIX_KEY = "METHOD_CACHE:";

  private final RedisTemplate<String, Object> redisTemplate;

  /* Config Gson for working with LocalDate/LocalDateTime */
  private static final Gson GSON =
      new GsonBuilder()
          .registerTypeAdapter(
              LocalDate.class,
              (JsonSerializer<LocalDate>)
                  (value, type, context) ->
                      new JsonPrimitive(value.format(DateTimeFormatter.ISO_LOCAL_DATE)))
          .registerTypeAdapter(
              LocalDateTime.class,
              (JsonSerializer<LocalDateTime>)
                  (value, type, context) ->
                      new JsonPrimitive(value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)))
          .registerTypeAdapter(
              LocalDate.class,
              (JsonDeserializer<LocalDate>)
                  (jsonElement, type, context) ->
                      LocalDate.parse(
                          jsonElement.getAsJsonPrimitive().getAsString(),
                          DateTimeFormatter.ISO_LOCAL_DATE))
          .registerTypeAdapter(
              LocalDateTime.class,
              (JsonDeserializer<LocalDateTime>)
                  (jsonElement, type, context) ->
                      LocalDateTime.parse(
                          jsonElement.getAsJsonPrimitive().getAsString(),
                          DateTimeFormatter.ISO_LOCAL_DATE_TIME))
          .create();

  /**
   * this method will be called with the method has annotation @SingletonCall if there are multiple
   * request calls at the same time, only first request can be processed other request must wait for
   * data from first request. Firstly, those request will check Redis cache, If having data (value
   * != LOCKED) return intermediately Or else, first request will call database and process, then
   * save data to redis cache Other requests will recall redis after each `callAfterMilis`, until
   * having data in redis This appoach will save CPU of database server in case expensive queries
   */
  @Around("@annotation(singletonCall)")
  public Object aroundAdvice(ProceedingJoinPoint joinPoint, SingletonCall singletonCall)
      throws Throwable {

    CodeSignature methodSignature = (CodeSignature) joinPoint.getSignature();
    methodSignature.getName();
    String[] sigParamNames = methodSignature.getParameterNames();
    Object[] sigParamValues = joinPoint.getArgs();
    String cacheKey = generateCacheKey(methodSignature.getName(), sigParamNames, sigParamValues);

    var opValuesRedis = redisTemplate.opsForValue();
    try {
      Object cacheResult = redisTemplate.opsForValue().get(cacheKey);
      if (cacheResult == null) {
        opValuesRedis.set(cacheKey, LOCKED);
        Object data = joinPoint.proceed();
        opValuesRedis.set(
            cacheKey, GSON.toJson(data), singletonCall.expireAfterSeconds(), TimeUnit.SECONDS);
        return data;
      } else {
        if (LOCKED.equals(cacheResult.toString())) {
          do {
            Thread.sleep(singletonCall.callAfterMilis());
            cacheResult = redisTemplate.opsForValue().get(cacheKey);
            if (cacheResult == null) {
              return null;
            }
          } while (LOCKED.equals(cacheResult.toString()));
        } else {
          return GSON.fromJson(cacheResult.toString(), singletonCall.typeToken().getType().get());
        }
        return GSON.fromJson(cacheResult.toString(), singletonCall.typeToken().getType().get());
      }
    } catch (Exception e) {
      redisTemplate.delete(cacheKey);
      throw e;
    } finally {
      // do nothing
    }
  }

  private String generateCacheKey(
      String methodName, String[] sigParamNames, Object[] sigParamValues) {
    StringBuilder str = new StringBuilder();
    for (int i = 0; i < sigParamNames.length; i++) {
      str.append(sigParamNames[i]).append(":").append(sigParamValues[i]);
      if (i < sigParamNames.length - 1) str.append("_");
    }
    return network + "-" + PREFIX_KEY + methodName + ":" + str.toString().hashCode();
  }
}
