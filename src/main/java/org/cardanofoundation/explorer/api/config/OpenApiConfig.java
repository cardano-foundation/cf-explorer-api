package org.cardanofoundation.explorer.api.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("!prod")
public class OpenApiConfig {

  @Bean
  public OpenAPI customOpenAPI() {
    final String securitySchemeNAme = "bearerAuth";
    return new OpenAPI()
        .addSecurityItem(
            new SecurityRequirement().addList(securitySchemeNAme))
        .components(
            new Components()
                .addSecuritySchemes(securitySchemeNAme,
                                    new SecurityScheme()
                                        .name(securitySchemeNAme)
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                )
        )
        .info(new Info().title("Iris API")
                  .description("Iris API OpenAPI 3.0")
                  .contact(new Contact()
                               .email("info@cardano.com")
                               .name("Cardano")
                               .url("https://cardano.org"))
//            .license(new License()
//                .name("Apache 2.0")
//                .url("http://www.apache.org/licenses/LICENSE-2.0.html"))
                  .version("1.0.0"));
  }
}
