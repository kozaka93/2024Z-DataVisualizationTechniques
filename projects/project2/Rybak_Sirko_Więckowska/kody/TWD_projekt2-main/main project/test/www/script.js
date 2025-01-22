const array = Array(7).fill(0);

function kliknijKwadrat(ktory) {
  
    let ktory_licznik = 0;
    let ktory_id = "";
    
    let ile_x = 0;
    let ile_y = 0;

    switch(ktory){
      case 1:
        ktory_id = "#first_cube";
        ile_x = 12;
        ile_y = 12;
        break;
      case 2:
        ktory_id = "#second_cube";
        ile_x = 11.5;
        ile_y = 12;
        break;
      case 3:
        ktory_id = "#third_cube";
        ile_x = 12;
        ile_y = 12;
        break;
      case 4:
        ktory_id = "#fourth_cube";
        ile_x = 12;
        ile_y = 12;
        break;
      case 5:
        ktory_id = "#fifth_cube";
        ile_x = 12;
        ile_y = 12;
        break;
      case 6:
        ktory_id = "#sixth_cube";
        ile_x = 12;
        ile_y = 12;
        break;
    }
    

    
    if(array[ktory] > 0){
      return;
    } 
    array[ktory] = 1;

    let currentAngleX = 0;
    let currentAngleY = 0;

    const kwadrat = document.querySelector(ktory_id);
    let style = window.getComputedStyle(kwadrat);
    let transform = style.getPropertyValue('transform');

    if (transform === 'none') {
        console.log('Kwadrat jest w stanie początkowym, brak obrotu.');
    } else {
        const matrix = transform.match(/^matrix3d\((.+)\)$/);
        
        if (matrix) {
            const values = matrix[1].split(', ').map(Number);

            let sina = values[6];
            let cosa = values[0];

            let alfa = Math.asin(sina);

            if (cosa < 0) {
                alfa = Math.PI - alfa;
            }

            currentAngleX = alfa;
            currentAngleY = alfa;

        } else {
            currentAngleX = Math.PI;
            currentAngleY = Math.PI;
        }
    }


    kwadrat.style.webkitTransform = transform;

    kwadrat.style.animation = `obracanieFast${ktory} 8s cubic-bezier(0.15, 0, 0.00001, 1) forwards`;
    
    const faceElements = document.querySelectorAll(`#cube-container-${ktory} .face`);

    setTimeout(function() {
      faceElements.forEach(function(face) {
        console.log("xdddd", ktory)
        face.style.backgroundColor = "rgba(255, 204, 0, 1)";
      });
    }, 8000);
    

    const styleSheet = document.styleSheets[0];
    styleSheet.insertRule(`
                @keyframes obracanieFast${ktory}{
                    from {
                        transform: rotateX(${currentAngleX}rad) rotateY(${currentAngleY}rad);
                    }
                    to {
                        transform: rotateX(${ile_x*Math.PI}rad) rotateY(${ile_y*Math.PI}rad);
                    }
                }
            `, styleSheet.cssRules.length);
}