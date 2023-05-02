package org.cardanofoundation.explorer.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

@SpringBootApplication
@EnableAspectJAutoProxy(proxyTargetClass = true)
//@EnableJpaAuditing
@EnableCaching
@EntityScan( {"org.cardanofoundation.explorer.*", "org.cardanofoundation.*"})
public class ExplorerApiApplication {

  public static void main(String[] args) {
    SpringApplication.run(ExplorerApiApplication.class, args);
  }

}
