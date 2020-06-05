/**********************************************************
 
list2artboard.jsx

DESCRIPTION

Rename artboards with a matching list of fruits.
 
**********************************************************/

//check if theres an active document
if (app.documents.length == 0) {  
    alert("Nenhum documento aberto/ativo");  
} else {  

    var doc = app.activeDocument;
    var userFruits = prompt("Nome dos artboards:", "pera, uva, maçã");
    var fruits = userFruits.split(", ");
    
    for (var i = 0, l = doc.artboards.length; i < l; i++) {
        var ab = doc.artboards[i];
        if (i < fruits.length) {
        	ab.name = fruits[i];  
        }
    }  
    alert("∧＿∧\n(｡･ω･｡)つ━☆・*。\n⊂　　 ノ　　　・゜+.\nしーＪ　　　°。+ *´¨)\n　　　　　　　　　.• ´¸.•*´¨) ¸.•*¨)\n　　　　　　　　　(¸.•´ (¸.•'* ☆ TEJE RENOMEADO");

} 