def host
def portDb
def usernameDb
def passwordDb
def environment
def envFileDeploy
def hostSonarqube
def projectKeyExplorerApi
def loginExplorerApi

def COLOR_MAP = [
    'SUCCESS': 'good',
    'FAILURE': 'danger',
]

pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                echo 'Building..'
                script {
                    def envFile
                    if (env.BRANCH_NAME == 'develop') {
                        envFile = readProperties file: '/tmp/env-explorer-api-dev.properties'
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
                    environment = envFile.environment
                    hostSonarqube = envFile.hostSonarqube
                    projectKeyExplorerApi = envFile.projectKeyExplorerApi
                    loginExplorerApi = envFile.loginExplorerApi
                }
                sh "mvn -DHOST=${host} -DPORT_DB=${portDb} -DUSERNAME_DB=${usernameDb} -DPASSWORD_DB=${passwordDb} -DSPRING_PROFILES_ACTIVE=${environment} -B -DskipTests clean package"
                echo 'Build successfully!!!'
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
                sh "mvn test -DHOST=${host} -DPORT_DB=${portDb} -DUSERNAME_DB=${usernameDb} -DPASSWORD_DB=${passwordDb} -DSPRING_PROFILES_ACTIVE=${environment}"
//              		sh "mvn clean verify -DskipITs=true';junit '**/target/surefire-reports/TEST-*.xml'archive 'target/*.jar'"
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
                    if (env.BRANCH_NAME == 'develop') {
                        envFileDeploy = '/tmp/env-explorer-api-dev.env'
                    }
                    if (env.BRANCH_NAME == 'test') {
                        envFileDeploy = '/tmp/env-test.env'
                    }
                    if (env.BRANCH_NAME == 'prod') {
                        envFileDeploy = '/tmp/env-prod.env'
                    }
                }
                sh "docker-compose --env-file ${envFileDeploy} up -d --build"
                sh "docker images -f 'dangling=true' -q --no-trunc | xargs docker rmi"
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
