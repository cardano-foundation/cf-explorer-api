package org.cardanofoundation.explorer.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableAspectJAutoProxy(proxyTargetClass = true)
//@EnableJpaAuditing
@EnableCaching
@EntityScan( {"org.cardanofoundation.explorer.*", "org.cardanofoundation.*"})
@EnableScheduling
public class ExplorerApiApplication {

  public static void main(String[] args) {
    SpringApplication.run(ExplorerApiApplication.class, args);
  }

}
