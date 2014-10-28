package site

import play.core.ApplicationProvider
import play.core.server.DefaultSSLEngineProvider
import javax.net.ssl.SSLEngine

class PoodleSSLEngineProvider(appProvider : ApplicationProvider) extends DefaultSSLEngineProvider(appProvider) {
  override def createSSLEngine: SSLEngine = {
    val engine = super.createSSLEngine
    engine.setEnabledProtocols(engine.getEnabledProtocols.filterNot(_.startsWith("SSL")))
    engine
  }
}
