def host
def portDb
def usernameDb
def passwordDb
def environment
def envFileDeploy
def composeFile
def hostSonarqube
def projectKeyExplorerApi
def loginExplorerApi
def redisSentinelPass
def redisMasterName
def redisSentinelHost
def db
def COLOR_MAP = [
    'SUCCESS': 'good',
    'FAILURE': 'danger',
]

pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                sh "mvn -B -DskipTests clean package"
                echo 'Build successfully!!!'
            }
        }
        stage('Test') {
            steps {
                script {
                    def envFile
                    if (env.BRANCH_NAME == 'develop') {
                        envFile = readProperties file: '/tmp/env-dev.properties'
                    }
                    if (env.BRANCH_NAME == 'test') {
                        envFile = readProperties file: '/tmp/env-test.properties'
                    }
                    if (env.BRANCH_NAME == 'prod') {
                        envFile = readProperties file: '/tmp/env-prod.properties'
                    }
                    host = envFile.host
                    portDb = envFile.portDb
                    usernameDb = envFile.usernameDb
                    passwordDb = envFile.passwordDb
                    db = envFile.db
                    environment = envFile.environment
                    hostSonarqube = envFile.hostSonarqube
                    projectKeyExplorerApi = envFile.projectKeyExplorerApi
                    loginExplorerApi = envFile.loginExplorerApi
                    redisSentinelPass = envFile.redisSentinelPass
                    redisMasterName = envFile.redisMasterName
                    redisSentinelHost = envFile.redisSentinelHost
                }
                echo 'Testing..'
                sh "mvn test -DHOST=${host} -DPORT_DB=${portDb} -DUSERNAME_DB=${usernameDb} -DPASSWORD_DB=${passwordDb} -DSPRING_PROFILES_ACTIVE=${environment} -DREDIS_SENTINEL_PASS=${redisSentinelPass} -DREDIS_MASTER_NAME=${redisMasterName} -DREDIS_SENTINEL_HOST=${redisSentinelHost} -DDB=${db}"
                echo 'Test successfully!!!'
            }
        }
        stage('Sonarqube Scan') {
		    when {
                branch 'develop'
            }
            steps {
                echo 'Sonarqube scanning...'
                sh "mvn sonar:sonar -Dsonar.projectKey=${projectKeyExplorerApi} -Dsonar.analysisCache.enabled=false -Dsonar.host.url=${hostSonarqube} -Dsonar.login=${loginExplorerApi} -Dsonar.sources=src/main/java/ -Dsonar.java.binaries=target/classes"
                echo 'Sonarqube scan successfully!!!'
            }
        }
        stage('Deploy') {
            steps {
                echo 'Deploying....'
                script {
                    def envMainnet = "/tmp/explorer-api-mainnet.env"
                    def envTestnet = "/tmp/explorer-api-testnet.env"
                    def envPreprod = "/tmp/explorer-api-preprod.env"
                    def envPreview = "/tmp/explorer-api-preview.env"

                    sh "docker-compose --env-file ${envMainnet}  -p mainnet up -d --build"
                    sh "docker-compose --env-file ${envTestnet}  -p testnet up -d --build"
                    sh "docker-compose --env-file ${envPreprod}  -p preprod up -d --build"
                    sh "docker-compose --env-file ${envPreview}  -p preview up -d --build"
                }
 
              

                sh "docker images -f 'dangling=true' -q --no-trunc | xargs --no-run-if-empty docker rmi"
                echo 'Deployment Done!!!'
            }
        }
    }
    post {
        always {
            script {
                Author_ID = sh(script: "git show -s --pretty=%an", returnStdout: true).trim()
                Author_Name = sh(script: "git show -s --pretty=%ae", returnStdout: true).trim()
            }
            slackSend channel: "#build-group-test",
                    color: COLOR_MAP[currentBuild.currentResult],
                    message: "*${currentBuild.currentResult}:* ${env.JOB_NAME} build ${env.BUILD_NUMBER} by commit author: ${Author_ID} \n More information at: ${env.BUILD_URL}"
        }
    }
}
