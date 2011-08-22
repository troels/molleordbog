package org.bifrost.molleordbog.remoteapi

import com.google.appengine.tools.remoteapi.{RemoteApiInstaller, RemoteApiOptions}

object RemoteHandler { 
  var installer: RemoteApiInstaller = null
  def setUp()  {
    val options = new RemoteApiOptions()
                    .server("localhost", 80)
                    .credentials("bn.troels@gmail.com", "")
    installer = new RemoteApiInstaller()
    installer.install(options)
  }

  def tearDown() { 
    if (installer != null) installer.uninstall()
  }

  def withRemoteHandler[T](body: => T): T = {
    setUp()
    try { 
      body
    } finally { 
      tearDown()
    }
  }
}
