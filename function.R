sum_even.function <- function(start, end){
  sum_even <- 0
  for(i in start:end){
    if(i%%2 == 0){
      sum_even <- sum_even + i
    }
  }
  print(sum_even)
}

tinh_tien_dien_v201903.function <- function(so_kw){
    muc1 = 1678
    muc2 = 1734
    muc3 = 2014
    muc4 = 2536
    muc5 = 2834
    muc6 = 2927

    bac50 = 50
    bac100 = 100

    if(so_kw <= 50){
        tien_dien = so_kw * muc1
    } else if(so_kw <= 100){
        tien_dien = bac50 * muc1 + (so_kw - bac50) * muc2
    } else if(so_kw <= 200){
        tien_dien = bac50 * muc1 + bac50 * muc2 + (so_kw - bac100) * muc3
    } else if(so_kw <= 300){
        tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + (so_kw - bac50 - bac50 - bac100) * muc4
    } else if(so_kw <= 400){
        tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + bac100 * muc4 + (so_km - bac50 - bac50 - bac100 - bac100) * muc5
    } else{
        tien_dien = bac50 * muc1 + bac50 * muc2 + bac100 * muc3 + bac100 * muc4 + bac100 * muc5 + (so_kw - bac50 - bac50 - bac100 - bac100 - bac100) * muc6
    }
    print(paste('Tien dien:', tien_dien, 'VND'))
}

oddeven.function <- function(n){
    if (n%%2==0){
        print(paste(n, 'is even number'))
    } else{
        print(paste(n, 'is odd number'))
    }
}
  
tienthuephong.function <- function(maso, songayluutru){
    if (maso == 1){
        giaphong <- 1000000
    } else if (maso == 2){
        giaphong <- 900000
    } else if (maso == 3){
        giaphong <- 850000
    } else if (maso == 4){
        giaphong <- 700000
    } else {
        giaphong <- 550000
    }
    
    if (songayluutru >= 7){
        tongtien <- songayluutru * giaphong * 0.8
    } else if (songayluutru >= 3){
        tongtien <- songayluutru * giaphong * 0.9
    } else {
        tongtien <- songayluutru * giaphong
    }
    print(paste('Tong tien:', tongtien))
}
  
onetwothree.function <- function(player1, player2){
    if (player1 == 'scissors'){
        if (player2 == 'scissors'){
            results <- 'the same'
        } else if (player2 == 'rock'){
            results <- 'player2 win'
        } else{
            results <- 'player1 win'
        }
    } else if (player1 == 'rock'){
        if (player2 == 'scissors'){
            results <- 'player1 win'
        } else if (player2 == 'rock'){
            results <- 'the same'
        } else{
            results <- 'player2 win'
        }
    } else{
        if (player2 == 'scissors'){
            results <- 'player2 win'
        } else if (player2 == 'rock'){
            results <- 'player1 win'
        } else{
            results <- 'the same'
        }
    }
}
  
tinhcuoc.function <- function(loaixe, sokm){
    toi_thieu_2km_bike <- 10000
    km_bike <- 3600
    
    toi_thieu_2km_send <- 15000
    km_send <- 4000
    
    tien_xe <- 0
    
    if (loaixe == 1){
        if (sokm <= 2){
            tien_xe = toi_thieu_2km_bike
        } else{
            tien_xe = toi_thieu_2km_bike + (sokm - 2)*km_bike
        }
    } else if (loaixe == 2){
        if (sokm <= 2){
            tien_xe = toi_thieu_2km_send
        } else{
            tien_xe = toi_thieu_2km_send + (sokm - 2)*km_send
        }
    } else {
        print('Chi co 2 loai la Go-Bike va Go-Send')
    }
    print(paste('Tien xe:',tien_xe))
}

bmi.function <- function(name, birthday, weight, height){
    bmi <- weight / (height * height)
    print(paste("Name:", name, ", Year of Birth:", birthday, ", Height:", format(round(height,2), nsmall=2), "(m), Weight:", format(round(weight,2), nsmall=2), "(kg), BMI:", format(round(bmi,2), nsmall=2)))
}
 
bill.function <- function(totalfb, tax, sv){
    vat <- totalfb * tax
    tip <- totalfb * sv
    total <- totalfb + vat + tip
    print(paste('Thue phai tra:', vat, ', Tip:', tip, ', Tong tien:', total))
}
  
saving.function <- function(laisuatnam, sotiengui, songaygui){
    laisuatngay <- (laisuatnam/365)/100
    tienlai <- (sotiengui * songaygui) * laisuatngay
    tongsotien <- sotiengui + tienlai
    print(paste('Tien lai:', tienlai, ', Tong tien:', tongsotien))
}
  
hinhtron.function <- function(dientich){
    pi <- 3.14
    r <- sqrt(dientich/pi)
    p <- 2 * pi * r
    print(paste('Ban kinh:', round(r,2), ', Chu vi:', round(p, 2)))
}
