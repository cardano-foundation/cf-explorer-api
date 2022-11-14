package com.cardano.explorer.config;

import com.sotatek.cardanocommonapi.utils.RsaUtils;
import java.security.PublicKey;
import javax.annotation.PostConstruct;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties("rsa.key")
@Getter
@Setter
public class RsaConfig {

  private String publicKeyFile;

  private PublicKey publicKey;

  @PostConstruct
  public void createRsaKey() {
    java.security.Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
    publicKey = RsaUtils.getPublicKey(publicKeyFile);
  }
}
