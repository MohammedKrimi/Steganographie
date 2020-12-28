import com.tncy.top.image.ImageWrapper
import math.min
import SSIM.pixel
object etape2ter {
  def pixel2(couleur: Char, image: ImageWrapper, i: Int, j: Int): String = {
    /*  Nom de la fonction : pixel
        entrée : couleur, image, i, j
        retour : valeur entière de la composante correspondante à la couleur 'couleur'.
    */
    var image2D: Array[Array[Int]] = image.getImage();
    var chaine : String = ""
    var biais : Int = 0
    if (couleur == 'T') {
      biais = 0
    }
    else if (couleur == 'R') {
      biais = 8
    }
    else if (couleur == 'G') {
      biais = 16
    }
    else {
      biais = 24
    }
    for (k <- 0 to 7) {
      chaine = chaine + (image2D(i)(j).toBinaryString)(k + biais);
    }
    return chaine
  }
  def binToInt(s: String): Int = {
    /*  Nom de la fonction : hexToInt
      entrée : Chaine de caractère, correspondant à un nombre en base 16
      retour : Entier, conversion du nombre héxadécimal de base.
   */
    s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
  }
  def recuperermessage(imageName:String, nbBit:Int, longueur:Int): Unit = {
    def nderniers(binaire:String,n:Int, nbBit:Int):String={ //on prends les nderniers caractères de l'entier n convertit en binaire
      var retour:String = ""
      var comptelocal : Int = 0
      for (k <- 0 to binaire.length-1){
        if(k > (7-nbBit) && comptelocal < n){ //si on est dans les n derniers caractères
          retour = retour + binaire(k)
          comptelocal = comptelocal + 1
        }
      }
      return retour
    }
    var imageSrc: ImageWrapper = new ImageWrapper(imageName) //création de la nouvelle variable image source, du type ImageWrapper
    var imageref: ImageWrapper = new ImageWrapper("image2.png") //création de la nouvelle variable image source, du type ImageWrapper
    var imageSrc2D: Array[Array[Int]] = imageSrc.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
    var imageFinale2DInt: Array[Array[Int]] = imageSrc.getImage(); //Création d'un tableau de tableau de la même taille que l'image d'origine, remplis initialement de 0
    //Parcours de l'image
    //A chaque pixel on prends les bits correspondants aux nbBit plus petits.
    var message:String = ""
    for (i <- 0 to imageSrc.height - 1) {
      for (j <- 0 to imageSrc.width - 1) {
          if((message.length) <= longueur-1) {
            message = message + nderniers(pixel2('R', imageSrc, i, j), min(nbBit,longueur-message.length), nbBit)
          }
          if((message.length) <= longueur-1) {
              message = message + nderniers(pixel2('G', imageSrc, i, j), min(nbBit,longueur-message.length), nbBit)
            }
          if((message.length) <= longueur-1) {
            message = message + nderniers(pixel2('B', imageSrc, i, j), min(nbBit,longueur-message.length), nbBit)
          }
      }
    }
    println("Votre message est : "+message)
  }
  def cachermessage(message:String, imageName:String, nbBit:Int): Unit = {
    def conversionbinaire(chaine:String):String={
      //Convertit une chaine de caractère en chaine binaire
      var binaire:Array[Byte] = chaine.getBytes("us-ascii")
      var retour:String = ""
      for (k<-0 to binaire.length-1){
        retour = retour + binaire(k).toBinaryString

      }
      println("La chaine que vous avez caché dans ce message était : "+retour)
      return retour
    }

    def separe(chaine:String, longueur:Int):Array[String]={
      //Sépare une chaine de caractère (binaire ici) en un certain nombre de morceau de longueur 'longueur'. (Si pas assez alors complète avec des 0)
      var compte:Int = chaine.length
      var approximation:Int = Math.ceil(compte.toDouble/longueur.toDouble).toInt
      var tabretour:Array[String] = Array.fill(approximation)("")
      var indiceRemplissage:Int = 0
      while (compte > 0 && indiceRemplissage< tabretour.length) //Tant qu'on a pas parcouru tout le message binaire
      {
        println(compte)
        var temporaire:String = ""
        var courrant : Int = longueur*indiceRemplissage //On définit la section courrante du message sur laquelle on travaille.
        for (k<-0 to min(longueur-1,compte-1)) {
          temporaire = temporaire + chaine(k + courrant)
        }
        for(k<-temporaire.length to longueur-1){ //On traîte le cas où le message ne peut pas être codé sur un nombre entier de pixel.
          temporaire = temporaire + "0"
        }
        println("À l'indice"+indiceRemplissage+" On ajoute : "+temporaire)
        tabretour(indiceRemplissage) = temporaire
        compte = compte - longueur
        indiceRemplissage = indiceRemplissage+1
      }
      return tabretour
    }
    def remplissagebin(couleur: Char, nbBit: Int, imageSrc2D: Array[Array[Int]], tableauChaine: Array[String], i: Int, j: Int):String= {

      var valeurstabstockées:Int = j
      var compteglobal:Int = j*3 + i*imageSrc2D.length
      var chaine: String = "";
      var biais: Int = 0
      if (couleur == 'R') {
        biais = 8
      }
      else if (couleur == 'G') {
        biais = 16
        compteglobal = compteglobal + 1
      }
      else if (couleur == 'B') {
        biais = 24
        compteglobal = compteglobal + 2
      }
      for (k <- 0 to 7) {
        if (compteglobal < tableauChaine.length) { //Si on a pas encore stocker tout le message binaire.
          if (k < (8-nbBit))
          {
            chaine = chaine + (imageSrc2D(i)(j).toBinaryString) (k + biais) //Si on est pas encore sur les bits où il faut cacher, on ajoute l'image normale
          }
          else
          {
            chaine = chaine + tableauChaine(compteglobal) //Si on est sur les bon bits, on envoit le tableauChaine constitué
            return chaine
          }
        }
        else { //Sinon, on remplit l'image normallement
          chaine = chaine + (imageSrc2D(i)(j).toBinaryString) (k + biais)
        }
      }
      return chaine;
    }
    var imageSrc: ImageWrapper = new ImageWrapper(imageName) //création de la nouvelle variable image source, du type ImageWrapper
    var imageSrc2D: Array[Array[Int]] = imageSrc.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
    var imageFinale2DInt: Array[Array[Int]] = imageSrc.getImage(); //Création d'un tableau de tableau de la même taille que l'image d'origine, remplis initialement de 0
    var chainebinaire : String  = message          //conversionbinaire(message) si on veut pouvoir prendre en compte un nombre binaire !!
    var tableauChaine : Array[String]= separe(chainebinaire, nbBit) // [01, 01, 11, 10 ... ]
    var compteglobal: Int = 0
    for (i <- 0 to imageSrc.height - 1) {
      for (j <- 0 to imageSrc.width - 1) {
        var chainefinale: String = "11111111" + remplissagebin('R', nbBit, imageSrc2D, tableauChaine, i, j) + remplissagebin('G', nbBit, imageSrc2D, tableauChaine, i, j) + remplissagebin('B', nbBit, imageSrc2D, tableauChaine, i, j)
        imageFinale2DInt(i)(j) = binToInt(chainefinale)
      }
    }

    println("Le message et l'image ont fusionnés dans un fichier nommé outputmessage.png")
    var outputFile: String = "outputmessage.png"
    imageSrc.saveImage(outputFile)
  }

}