package org.cardanofoundation.explorer.api.config.datasource;

import java.lang.reflect.Method;

import lombok.extern.log4j.Log4j2;

import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;

@Aspect
@Log4j2
@Component
@Order(-20)
public class DataSourceAspect {

  @Pointcut("@annotation(SwitchDataSource)")
  public void annotationPointCut() {
    //default function
  }

  @Before("annotationPointCut()")
  public void before(JoinPoint joinPoint) {
    MethodSignature sign = (MethodSignature) joinPoint.getSignature();
    Method method = sign.getMethod();
    SwitchDataSource annotation = method.getAnnotation(SwitchDataSource.class);
    if (annotation != null) {
      AbstractRoutingDataSourceImpl.setRoute(annotation.value());
      log.info("Switch DataSource to [{}] in Method [{}]", annotation.value(),
               joinPoint.getSignature());
    }
  }

  @After("annotationPointCut()")
  public void after(JoinPoint point) {
    if (null != AbstractRoutingDataSourceImpl.getRoute()) {
      log.info("Clear DataSource to [{}] after Method [{}]",
               AbstractRoutingDataSourceImpl.getRoute(), point.getSignature());
      AbstractRoutingDataSourceImpl.clearRoute();
    }
  }
}
