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
    log.info("Method {} has started!!!!!", joinPoint.getSignature().getName());
    try {
      long startTime = System.currentTimeMillis();
      Object value = joinPoint.proceed();
      long endTime = System.currentTimeMillis();
      log.info("Method {} running in {} ms",
          joinPoint.getSignature().getName(), (endTime - startTime));
      return value;
    } finally {
      log.info("Method {} has ended!!!!!", joinPoint.getSignature().getName());
    }
  }
}
