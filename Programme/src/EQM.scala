import com.tncy.top.image.ImageWrapper

import scala.math.{log, min, pow};
object EQM extends App {

  def hexToInt(s: String): Int = {
    s.toList.map("0123456789abcdef".indexOf(_)).reduceLeft(_ * 16 + _);
  }
  def logBasey(x:Double, y:Double) : Double = {
     return log(x) / log(y)
  }

  var imageSrcName: String = "paysage.png"   //Nom de la première image
  var imageDestName: String = "outputImage.png"    //Nom de la seconde image

  var imageSrc: ImageWrapper = new ImageWrapper(imageSrcName) //création de la nouvelle variable image source, du type ImageWrapper
  var imageDest: ImageWrapper = new ImageWrapper(imageDestName) //Idem pour l'image de destination.

  var imageSrc2D: Array[Array[Int]] = imageSrc.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
  var imageDest2D : Array[Array[Int]] = imageDest.getImage();//Idem pour l'image de destination.

  var imageSrc2DHex : Array[Array[String]] = Array.fill(imageSrc.height,imageSrc.width)("0")
  var imageDest2DHex : Array[Array[String]] = Array.fill(imageSrc.height,imageSrc.width)("0")

  for (i <- 0 to imageSrc.height-1) {
    for (j <- 0 to imageDest.width-1) {
      imageSrc2DHex(i)(j) = imageSrc2D(i)(j).toHexString
    }
  }
  for (i <- 0 to imageDest.height-1) {
    for (j <- 0 to imageDest.width-1) {
      imageDest2DHex(i)(j) = imageDest2D(i)(j).toHexString
    }
  }


  def eqm(imageoriginale2D : Array[Array[String]], imagedest2D : Array[Array[String]],imagecache2D : Array[Array[String]]) : Double = {
    val n=min(imageoriginale2D.length,imagedest2D.length)-1
    val m=min(imagecache2D(0).length,imagedest2D(0).length)-1
    var eqm : Double = 0 ;


    for (i<- 1 to n) {
      for (j<-1 to m) {
        eqm=eqm + pow(hexToInt(imageoriginale2D(i)(j)) - hexToInt(imagecache2D(i)(j)),2)
      }
    }
    var eqm1 : Double = 1;
    eqm1=eqm/(n*m)
    eqm1
  }

  def psnr(imageoriginale2D : Array[Array[String]], imagedest2D : Array[Array[String]], imagecache2D : Array[Array[String]]): Double = {
    val a=eqm(imageoriginale2D,imagedest2D,imagecache2D)
    val d=pow(2,24);
    val b=10*logBasey(pow(d,2)/a,10)
    b
  }
  //val valeur1=eqm(imageSrc2DHex,imageDest2DHex)
  //val valeur2=psnr(imageSrc2DHex,imageDest2DHex)
  //println("Valeur EQM  : " +valeur1 )
  //println("Valeur PSNR : " +valeur2 )
}