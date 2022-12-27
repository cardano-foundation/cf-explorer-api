package com.cardano.explorer.config;

import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Component
@Log4j2
public class LoggerAspect {

  @Around("@annotation(LogMessage)")
  public Object aroundAdvice(ProceedingJoinPoint joinPoint) throws Throwable {
    String methodName = joinPoint.getSignature().getName();
    String className = joinPoint.getTarget().getClass().getName();
    log.info("Method {} in {} has started!!!!!", methodName, className);
    try {
      long startTime = System.currentTimeMillis();
      Object value = joinPoint.proceed();
      long endTime = System.currentTimeMillis();
      log.info("Method {} in {} running in {} ms",methodName, className, (endTime - startTime));
      return value;
    } finally {
      log.info("Method {} in {} has ended!!!!!", methodName, className);
    }
  }
}
