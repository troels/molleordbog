package org.bifrost.molleordbog.remoteapi

import com.google.appengine.tools.remoteapi.{RemoteApiInstaller, RemoteApiOptions}

object RemoteHandler { 
  var installer: RemoteApiInstaller = null
  def setUp()  {
    val options = new RemoteApiOptions()
                    .server("localhost", 8080)
                    .credentials("bn.troels", "")
    installer = new RemoteApiInstaller()
    installer.install(options)
  }

  def tearDown() { 
    if (installer == null) return
    installer.uninstall()
  }
}
