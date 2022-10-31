package com.cardano.explorer;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableAspectJAutoProxy(proxyTargetClass=true)
@EnableJpaAuditing
public class ExplorerApiApplication {

	public static void main(String[] args) {
		SpringApplication.run(ExplorerApiApplication.class, args);
	}

}
