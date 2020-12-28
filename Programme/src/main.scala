import decode.decod
import encode.encod
import etape2EQM.encodeqm
import etape2ssimfinale.encodssim
import placerest.placerestante
import etape2ter.cachermessage
import etape2ter.recuperermessage

object main extends App{
  def choix() {
    println("Que voulez-vous faire ?")
    println("O : Cacher une image dans une autre")
    println("1 : Séparer deux images")
    println("2 : Estimer la place disponible " )
    println("3 : Cacher un message binaire")
    println("4 : Récupérer un message binaire")
    println("5 : Quitter")
    println("Veuillez entrer le numéro correspondant à votre requête.")
    val reponse = readLine("Réponse : ")
    reponse match {
      case "0" => choixencod
      case "1" => decod()
      case "2" => var imageSrcName: String = readLine("Quel est le nom de l'image dont il faut estimer la place disponible ?") //Nom de la première image
                  placerestante(imageSrcName)
      case "3" => var message: String = readLine("Quel message binaire voulez-vous cacher ?")
                  var image: String = readLine("Dans quel image voulez-vous cacher ce message ?")
                  var nbBit: String = readLine("Sur combien de bits voulez-vous cacher ce message ?")
                  cachermessage(message, image, nbBit.toInt)
      case "4" => var image: String = readLine("Dans quel image voulez-vous decrypter un message ?")
                  var nbBit: String = readLine("Sur combien de bits ce message a-t'il été caché ?")
                  var longueur: String = readLine("Quelle est la longueur de la chaine recherchée ?")
                  recuperermessage(image, nbBit.toInt,longueur.toInt)
      case _ => print("Veuillez choisir une des options disponibles : 0, 1 , 2 , 3 , 5 "); println ; choix()
    }
  }
  def choixencod(): Unit = {
    println("Veuillez placer vos deux images dans le même dossier que le projet")
    println("Quelle méthode voulez-vous utiliser ?")
    println("EQM : Méthode EQM " )
    println("SSIM : Méthode SSIM")
    println("MB : Méthode brute")
    println("R : Retour")
    val reponse = readLine("Réponse : ")
    reponse match {
      case "EQM"  => encodeqm()
      case "SSIM" => encodssim()
      case "MB"    =>  var imageSrcName: String = readLine("Quel le nom de l'image innocente ? ") //Nom de la première image
                      var imageDestName: String = readLine("Quel est le nom de l'image à cacher ? ") //Nom de la seconde image
                      var nbBit: Int = readLine("Sur combien de bits voulez-vous encoder votre image a cacher (entier entre (1-8)?)").toInt //Nom de la seconde image
                      nbBit = 8 - nbBit;
                      encod(imageSrcName, imageDestName, nbBit)
      case "R"    => choix()
      case _ => print("Veuillez choisir 0, 1 ou 2"); println; choixencod()
    }
  }
  choix()
}