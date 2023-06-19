package org.cardanofoundation.explorer.api.config.aop.singletoncache;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface SingletonCall {

  /**
   * Time to live of cache value in redis
   **/
  int expireAfterSeconds() default 30; //in second

  /**
   * Time to recall redis cache to check locked release
   **/
  int callAfterMilis() default 500; //in milisecond

  /**
   * Type for Gson deserialized
   **/
  TypeTokenGson typeToken();

}
