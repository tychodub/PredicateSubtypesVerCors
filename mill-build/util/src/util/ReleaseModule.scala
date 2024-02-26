package util

import mill.{PathRef, T, pathReadWrite}
import os.Path

trait ReleaseModule extends JavaModule with SeparatePackedResourcesModule {
  def name: T[String] = T { this.getClass.getSimpleName.replace("$", "").capitalize }
  def executableName: T[String] = T { name().toLowerCase }
  def version: T[String] = T { "0.1-SNAPSHOT" }
  def maintainer: T[String] = T { "Unknown <unknown@example.com>" }
  def summary: T[String] = T { s"${name()} test build" }
  def description: T[String] = T { s"${name()} test build" }
  def homepage: T[Option[String]] = T { None }

  def debianPackageName: T[String] = T { name().toLowerCase }
  def debianSection: T[String] = T { "java" }
  def debianArchitecture: T[String] = T { "all" }

  def winExecutableName: T[String] = T { executableName() + ".bat" }

  private def copy(from: Path, to: Path): Path = {
    os.copy(from, to, followLinks = true, replaceExisting = false, createFolders = true, mergeFolders = true)
    to
  }

  def release() = T.command {
    Map(
      "unix" -> unixTar().path,
      "macos" -> macosTar().path,
      "win" -> winZip().path,
      "deb" -> deb().path,
    )
  }

  def unixTar = T {
    val dest = T.dest / "dest"

    val jar = copy(assembly().path, dest / s"${executableName()}.jar")
    val res = bareClasspath().map(res => copy(res, dest / res.last))

    os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)
    os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

    os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

    os.write(dest / executableName(),
      s"""#!/bin/sh
         |HERE=$$(dirname $$(readlink -f $$0))
         |(cd $$HERE; java ${forkArgs().mkString(" ")} @${os.rel / ".classpath"} ${finalMainClass()} "$$@")
         |""".stripMargin)
    os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

    val out = T.dest / s"${executableName()}-${version()}-unix.tar.xz"
    os.proc("tar", "-cJf", out, os.list(dest).map(_.relativeTo(dest))).call(cwd=dest)
    PathRef(out)
  }

  def macosTar = T {
    val dest = T.dest / "dest"

    val jar = copy(assembly().path, dest / s"${executableName()}.jar")
    val res = bareClasspath().map(res => copy(res, dest / res.last))

    os.walk(dest / "deps" / "unix", preOrder = false).foreach(os.remove)
    os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)

    os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

    os.write(dest / executableName(),
      s"""#!/bin/sh
         |HERE=$$(dirname $$(readlink -f $$0))
         |(cd $$HERE; java ${forkArgs().mkString(" ")} @${os.rel / ".classpath"} ${finalMainClass()} "$$@")
         |""".stripMargin)
    os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

    val out = T.dest / s"${executableName()}-${version()}-macos.tar.xz"
    os.proc("tar", "-cJf", out, os.list(dest).map(_.relativeTo(dest))).call(cwd = dest)
    PathRef(out)
  }

  def winZip = T {
    val dest = T.dest / "dest"

    val jar = copy(assembly().path, dest / s"${executableName()}.jar")
    val res = bareClasspath().map(res => copy(res, dest / res.last))

    os.walk(dest / "deps" / "unix", preOrder = false).foreach(os.remove)
    os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

    os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

    os.write(dest / winExecutableName(),
      s"""@echo off
         |cd /D "%~dp0"
         |java ${forkArgs().mkString(" ")} @${(os.rel / ".classpath").toString} ${finalMainClass()} %*
         |""".stripMargin)

    val out = T.dest / s"${executableName()}-${version()}-win.zip"
    os.proc("zip", out, "-r", os.list(dest).map(_.relativeTo(dest))).call(cwd = dest)
    PathRef(out)
  }

  def deb = T {
    val outName = s"${debianPackageName()}-${version()}-debian"
    val root = T.dest / outName
    os.makeDir(root)
    val dest = root / "usr" / "share" / debianPackageName()
    val fromDebRoot = (p: Path) => os.root / p.relativeTo(root)

    val jar = copy(assembly().path, dest / s"${executableName()}.jar")
    val res = bareClasspath().map(res => copy(res, dest / res.last))

    os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)
    os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

    os.write(dest / ".classpath", "-cp " + (jar +: res).map(fromDebRoot).map(_.toString).mkString(":"))

    os.write(dest / executableName(),
      s"""#!/bin/sh
         |java ${forkArgs().mkString(" ")} @${fromDebRoot(dest / ".classpath")} ${finalMainClass()} "$$@"
         |""".stripMargin)
    os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

    os.makeDir(root / "usr" / "bin")
    os.symlink(root / "usr" / "bin" / executableName(), fromDebRoot(dest / executableName()))

    os.makeDir(root / "DEBIAN")
    os.write(root / "DEBIAN" / "control",
      s"""Package: ${debianPackageName()}
         |Version: ${version()}
         |Section: ${debianSection()}
         |Architecture: ${debianArchitecture()}
         |Maintainer: ${maintainer()}${homepage().map(homepage => s"\nHomepage: $homepage").getOrElse("")}
         |Description: ${summary()}
         |${description().split('\n').mkString(" ", "\n ", "")}
         |""".stripMargin)

    os.proc("dpkg-deb", "--root-owner-group", "-Zxz", "--build", root.relativeTo(T.dest)).call(cwd = T.dest)
    PathRef(T.dest / s"$outName.deb")
  }
}