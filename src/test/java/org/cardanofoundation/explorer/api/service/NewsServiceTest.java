package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.nio.charset.StandardCharsets;
import java.util.function.Function;
import java.util.function.Predicate;

import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.service.impl.NewsServiceImpl;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
public class NewsServiceTest {
  @InjectMocks private NewsServiceImpl newsService;
  @Mock private WebClient webClient;
  @Mock private WebClient.RequestHeadersSpec requestHeadersMock;
  @Mock private WebClient.RequestHeadersUriSpec requestHeadersUriMock;
  @Mock private CustomMinimalForTestResponseSpec responseMock;
  @Mock private ResponseSpecWithException responseExceptionMock;

  ObjectMapper objectMapper = new ObjectMapper();

  @BeforeEach
  void setup() {
    ReflectionTestUtils.setField(newsService, "apiNewsDataUrl", "localhost:8080");
  }

  @Test
  void testGetNews_thenReturn() throws JsonProcessingException {
    Integer limit = 1;
    Integer offset = 0;
    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(String.format("localhost:8080", limit, offset)))
        .thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class), any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Object.class)).thenReturn(Mono.just(preNewsData()));

    var response = newsService.getNews(limit, offset);
    JsonNode jsonNode = objectMapper.valueToTree(response);
    Assertions.assertEquals(jsonNode.get("offset").asText(), "0");
    Assertions.assertEquals(jsonNode.get("articles").get(0).get("language").asText(), "en");
  }

  @Test
  void testGetNews_shouldThrowException() {
    Integer limit = 1;
    Integer offset = 0;
    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(String.format("localhost:8080", limit, offset)))
        .thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(500));
    when(responseMock.onStatus(any(Predicate.class), any(Function.class))).thenCallRealMethod();

    Assertions.assertThrows(BusinessException.class, () -> newsService.getNews(limit, offset));
  }

  Object preNewsData() throws JsonProcessingException {
    String newsData =
        "{\n"
            + "    \"total\": 119,\n"
            + "    \"offset\": 0,\n"
            + "    \"limit\": 1,\n"
            + "    \"articles\": [\n"
            + "        {\n"
            + "            \"title\": \"A Spotlight on Stake Pools: AHLNET\",\n"
            + "            \"language\": \"en\",\n"
            + "            \"banner_image\": \"https://ucarecdn.com/7e58e3c5-e707-4017-a7e5-9dd5eb404ede/\",\n"
            + "            \"meta_image\": \"https://ucarecdn.com/952dfd35-7ce5-4b6c-8316-836c73ffcf78/\",\n"
            + "            \"thumbnail_image\": \"https://ucarecdn.com/981d4a8e-1a5c-4166-84ce-c00ee543080c/\",\n"
            + "            \"layout\": \"news\",\n"
            + "            \"news_item_content\": {\n"
            + "                \"show_excerpt\": \"true\",\n"
            + "                \"date\": \"2024-01-22T18:38:49.986Z\",\n"
            + "                \"author\": \"https://ucarecdn.com/f706dc43-da98-4164-a05e-45ba5ca15adb/-/crop/1063x1413/0,0/-/preview/|Denicio Mac Kenzie Bute~~~Community Manager - Netherlands\",\n"
            + "                \"body\": \"The Cardano Foundation’s “A Spotlight on Stake Pools” series focuses on the crucial role stake pool operators (SPOs) play in the Cardano ecosystem. Each installment provides insights from a specific SPO and highlights its initiatives, projects, and commitment to open source practices. In the previous entry, we explored how [Hazelpool](https://cardanofoundation.org/en/news/a-spotlight-on-stake-pools-hazelpool/) supports the Cardano network while encouraging feline fostering and raising the profile of no-kill shelters. As Hazelpool has grown, so too has its ecosystem, expanding to include tools like the HAZELnet Discord bot. Users can access this tool at no cost by delegating their stake, further incentivizing decentralization efforts. In this installment, we now feature the [AHLNET stake pool](https://ahlnet.nu/), founded by Sweden-based Ola Ahlman. With over 15 years of IT management experience and expertise deploying mission-critical servers in an air traffic control center, Ahlman combines his technical expertise with a passion for working on private and open source projects in Java and other programming languages. Beyond the digital realm, Ahlman enjoys exploring distant corners of the world, immersing himself in local traditions and lifestyles. Ahlman’s travels have cultivated an awareness of social, economic, and ecological issues that shape his worldview. These perspectives align with his mission to amplify Cardano’s message and objectives by operating a stake pool, therefore strengthening network resilience and decentralization. Ahlam’s contributions to Cardano have led to a community vote designating AHLNET as an official stake pool for [SundaeSwap ISO and Scooper](https://sundae.fi/). Ahlman is also a core developer of [Eternl](https://eternl.io/app/mainnet/welcome), a popular Cardano community wallet. ### Why did you become a stake pool operator? When I first came into contact with blockchain back in 2017, it took some time to get a feeling for the crypto landscape, the goals, and the motivations behind each project. Once I stumbled upon Cardano, it didn't take long before I knew that this was the place to set roots and go long. I have always loved to travel the world beyond the normal tourist destinations to actually get a feeling for the local lifestyle, both for their wonders but also, in particular, the challenges and problems that exist around the world. Cardano with its mission to not only be a tool for the established economies but rather really focus on solving the real issues for supply chain verification, banking the unbanked, etc., really hit home. Because of this, I started reading up on the technical aspects of Cardano and how I could get involved to further this vision. I then joined the community channels on [Telegram](https://t.me/CardanoAnnouncements) and the [Cardano Forum](https://forum.cardano.org/) to see how I best could contribute with my particular skill set. This was still in the Early days of Cardano before stake pools were a thing, but from here, the idea of running a Cardano stake pool started to take root. ### What is the mission of your stake pool? With a long technical background in IT/server operation and management, I felt that I could contribute to the overall stability of the network by providing a Cardano stake pool that community users can rely on for optimal operation. A big goal from the start was that I would do so without taking the easy route and getting myself a hosted solution. Instead, everything should be self-hosted. This means running on bare-metal hardware on its own infrastructure (network) to distance myself from clustering all pool operations to popular centralized locations on the globe. As I'm from Sweden, this is where all my Cardano nodes, core block producer, and relays are deployed. Running on my own hardware comes with multiple challenges, especially related to redundancy for power and network (ISP) issues outside of my control. All of this is taken care of by careful design decisions, known to be stable infrastructure in my area of operation, and own built software/scripts for automatic failover in case of a problem. Another early goal of mine in running a stake pool was to run a single stake pool. This allows for maximum chain decentralization and makes room for more successful and sustainable operations. ### Can you give us a brief history of your stake pool? My Cardano journey started by helping out with running and testing the pre-release versions of the first Cardano node software to be used for the ITN (Incentivized TestNet). Once in a \\\"stable\\\" state, the dates around the launch of the ITN started to take form. It was time to start thinking of a brand for my pool. Due to being horrible at this stuff, I made it simple: I just went with the first part of my family name AHL, and this became my pool ticker. The ITN started rough, with many sleepless nights due to a few stability issues in the node software. A lot of tinkering with failover scripts, etc. I didn't manage to attract many delegators to my pool for the first half of the ITN. A bit of a bummer, but gave me the time to think about how I could better contribute. During the ITN days, I made several new long-lasting friends in the SPO community. Mainnet launch approached with the switch from Rust to Haskell code base. I was invited to the Friends & Family group to be one of the first to iron out bugs and help with documentation to make the onboarding for the rest of the SPO community as painless as possible. I continued with the AHL (AHLNET) brand into the mainnet launch, which was the 15th first pool registration on the blockchain and has been running non-stop since. ### How did the Cardano Foundation delegation impact your stake pool operation? Marketing and social engagement have never been my thing even though I recognize that it's a crucial part of being a stake pool operator. Time is finite and I want to spend as much of it as possible contributing where I can do the most good, and that is on the tech. This is where the Cardano Foundation delegation program really helped. Right off the bat for the mainnet launch, I received a healthy boost to kickstart my pool for my contributions to the [SPO Guild Operators toolset](https://cardano-community.github.io/guild-operators). This helped to put AHL on the map and showed delegators how their stake contributes to the overall health of the ecosystem. I have since been selected for a couple more Cardano Foundation delegation rounds, and this has been crucial for me to keep a healthy saturation level for my devoted and loyal delegators who have stuck with me through thick and thin. Because of this and due to a very stable pool setup with redundancy failover scripts, I have been able to spend most of my time building useful tools instead. My goal has always been to be able to use the revenue from the Cardano stake pool operation to fund what I really love to do: build stuff. ### Did you find the Cardano Foundation delegation enabled you to build or improve your tools, projects, or open source repositories? During the ITN days, I got to know many other early pioneers in the Cardano SPO space. For example, I ran into Andrew behind the [BCSH stake pool](https://bluecheesestakehouse.com/), who created [Jormanager](https://bitbucket.org/muamw10/jormanager/src/develop/) to manage a cluster of Jormungandr (ITN node sw) nodes. To help fellow SPOs install this great tool, I created a bash installation script for Jormanager to guide and set up dependencies, configuration, and services needed. This was the start of my deep dive into the wonders of bash. When the ITN end was in sight and mainnet, with its new Haskell code base, started to take shape, I joined the [Guild Operators group](https://github.com/cardano-community/guild-operators). A group of established figures in the space with a deep commitment to helping bring clarity and solutions to technical questions and issues. Under the Guild Ops umbrella, I helped develop many useful open source easy-to-use tools like [CNTools](https://cardano-community.github.io/guild-operators/Scripts/cntools/), [gLiveView](https://cardano-community.github.io/guild-operators/Scripts/gliveview/), and other utility scripts for the operation, monitoring, and management of a Cardano stake pool. The tools were ready in a stable state for the mainnet launch and helped many pool operators with a successful launch. Since then and to this day, this tool set has grown a lot with new features added to adapt to the evolving Cardano blockchain with additions of native tokens and much more. It's great to follow the adoption and how widespread these tools are today in the SPO community. Another pool operator I ran into during the ITN days and later again after mainnet launch, was Marcel running the [TITAN stake pool](https://www.titanstaking.io/). We both saw a need in the space for a Cardano wallet that could do more than was provided by the wallets at the time. Both Daedalus and Yoroi were too limited for our needs and had issues we felt we could solve. This became the start of a long friendship and collaboration with the launch of CCVault back in 2021. Since then, it has seen two rebrands due to name conflicts, with Eternl wallet now as the established and final brand. It's wonderful to see the recognition we have got in the space, with total transactions on the Cardano blockchain sent through Eternl going over 50% at times. The icing on the cake was the award received during the [Cardano Summit 2022](https://summit.cardano.org/archive/lausanne/) for [best wallet](https://www.youtube.com/watch?v=mb252CwjHOE) voted by the community. It fuels the team to work even harder to continue to innovate and provide the best wallet experience. While working on the Guild Ops tools and Eternl wallet, another missing piece was identified. A good API query layer to help builders on Cardano fetch the data they need to create applications on top of Cardano. A few from the Guild Ops group came together to solve this, and [Koios](https://koios.rest) was born, a distributed and [open source public API query layer](https://github.com/koios-official) for Cardano that is elastic in nature and provides an easy-to-query RESTful layer that has a lot of flexibility to cater to different data consumption requirements from the blockchain. To be able to spend the time needed on all these parallel ongoing projects, I stepped down to only working 50% at my non-blockchain day job at an air traffic control center here in Sweden. As you might expect, with a day job, running a stake pool, and participating in many different ongoing projects building on Cardano, the schedule is full, but I love it and wouldn't have it any other way. To stay updated on AHLNET, follow [@olaahlman](https://twitter.com/olaahlman) on X (formerly known as Twitter), or join the [AHLNET Telegram](https://t.me/AHLNETchat) group. The Cardano Foundation encourages all new stake pools, as well as those pools with novel initiatives, to submit the [single form](https://cardanocommunity.typeform.com/cf-delegation).\",\n"
            + "                \"cat\": \"all\",\n"
            + "                \"image\": \"https://ucarecdn.com/ec6baeaf-f3d3-4ca2-82fb-7a2b4f28a915/\",\n"
            + "                \"default_content\": \"We now feature the AHLNET stake pool, founded by Sweden-based Ola Ahlman. Ola explains why he became a stake pool operator and the mission of the stake pool.\"\n"
            + "            },\n"
            + "            \"name\": \"a-spotlight-on-stake-pools-ahlnet.md\",\n"
            + "            \"sha\": \"4571d304225583fc80f8a6cc92f629898b01f8ff\",\n"
            + "            \"resource_href\": \"https://cardanofoundation.org/en/news/a-spotlight-on-stake-pools-ahlnet/\"\n"
            + "        }\n"
            + "    ]\n"
            + "}";
    return objectMapper.readValue(newsData, Object.class);
  }

  abstract class CustomMinimalForTestResponseSpec implements WebClient.ResponseSpec {
    public abstract HttpStatusCode getStatusCode();

    public WebClient.ResponseSpec onStatus(
        Predicate<HttpStatusCode> statusPredicate,
        Function<ClientResponse, Mono<? extends Throwable>> exceptionFunction) {
      if (statusPredicate.test(this.getStatusCode()))
        exceptionFunction.apply(ClientResponse.create(HttpStatus.OK).build()).block();
      return this;
    }
  }

  abstract class ResponseSpecWithException implements WebClient.ResponseSpec {
    public abstract HttpStatusCode getStatusCode();

    public WebClient.ResponseSpec onStatus(
        Predicate<HttpStatusCode> statusPredicate,
        Function<ClientResponse, Mono<? extends Throwable>> exceptionFunction) {
      if (statusPredicate.test(this.getStatusCode()))
        exceptionFunction.apply(ClientResponse.create(HttpStatus.OK).build()).block();
      return this;
    }
  }
}
