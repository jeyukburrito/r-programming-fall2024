
# 준비

library(beepr)

# 게임 화면 크기, 동그라미 크기, 개수 등 기본 파라미터
game_size <- 2       # 게임 화면 크기
circle_size <- 0.03  # 동그라미(아군/적군) 크기
n_friends <- 15       # 아군 동그라미 개수
n_enemies <- 15       # 적군 동그라미 개수
game_time <- 20      # 게임 시간 (초)
move_range <- 0.05   # 동그라미 초기 이동 범위
speed_increase <- 0.04       # 아군 속도 증가량
enemy_speed_increase <- 0.06 # 적군 속도 증가량


Unif_on_Circle <- function(a, b, r, n) {
  x <- runif(n, a - r, a + r)
  y <- numeric(n)
  for (i in seq_len(n)) {
    y[i] <- runif(1,
                  -sqrt(r^2 - (x[i] - a)^2) + b,
                  sqrt(r^2 - (x[i] - a)^2) + b)
  }
  cbind(x, y)
}


# 기본 폭죽 함수 (게임 승리 / 마지막 연출 시 사용)

Fireworks <- function(num_fire = 5, num_points = 200,
                      steps = 10, sleep = 0.1,
                      color_alpha = 0.5,
                      x_min = -1, x_max = 1,
                      y_min = -1, y_max = 1) {
  
  # 폭죽 중심 좌표
  center_ab <- cbind(runif(num_fire, x_min, x_max),
                     runif(num_fire, y_min, y_max))
  
  # 폭죽별 반지름(폭발 범위)
  r_vec <- runif(num_fire, 0.1, 0.3)
  
  # 폭죽이 터질 점들(원 내부 임의 좌표들)
  fire_points_list <- lapply(seq_len(num_fire), function(i) {
    Unif_on_Circle(center_ab[i, 1], center_ab[i, 2], r_vec[i], num_points)
  })
  
  # 색상 팔레트 (alpha 포함)
  base_colors <- rainbow(num_points, alpha = color_alpha)
  
  # 단계별로 퍼져나가는 애니메이션
  for (i in seq_len(num_fire)) {
    cx <- center_ab[i, 1]
    cy <- center_ab[i, 2]
    fire_points <- fire_points_list[[i]]
    color_vec <- sample(base_colors, num_points)
    
    for (step_i in seq_len(steps)) {
      expand_ratio <- step_i / steps
      for (j in seq_len(num_points)) {
        x_new <- cx + expand_ratio * (fire_points[j, 1] - cx)
        y_new <- cy + expand_ratio * (fire_points[j, 2] - cy)
        
        # 중심->새 위치 선 그리기
        lines(x = c(cx, x_new), y = c(cy, y_new),
              col = color_vec[j], lwd = 1)
        
        if (step_i == steps) {
          points(x_new, y_new, pch = 16, col = color_vec[j], cex = 0.7)
        }
      }
      Sys.sleep(sleep)   
    }
  }
}


# 폭탄(빨간 폭죽) 터지는 함수 (게임 패배 시 아군 아바타 근처)

BombExplosion <- function(center_x, center_y, radius = 0.2,
                          times = 3,
                          num_points = 100,
                          steps = 8,
                          sleep = 0.07) {
  for (rep_i in seq_len(times)) {
    center_ab <- cbind(runif(1, center_x - 0.03, center_x + 0.03),
                       runif(1, center_y - 0.03, center_y + 0.03))
    fire_points <- Unif_on_Circle(center_ab[1, 1], center_ab[1, 2], radius, num_points)
    
    for (step_i in seq_len(steps)) {
      expand_ratio <- step_i / steps
      for (j in seq_len(num_points)) {
        x_new <- center_ab[1, 1] + expand_ratio * (fire_points[j, 1] - center_ab[1, 1])
        y_new <- center_ab[1, 2] + expand_ratio * (fire_points[j, 2] - center_ab[1, 2])
        
        # 선 그리기 (빨간색)
        lines(x = c(center_ab[1, 1], x_new),
              y = c(center_ab[1, 2], y_new),
              col = rgb(1, 0, 0, 0.6), lwd = 2)
        
        # 마지막 단계에는 점을 더 크게
        if (step_i == steps) {
          points(x_new, y_new, pch = 16, col = rgb(1,0,0,0.8), cex = 0.9)
        }
      }
      Sys.sleep(sleep)   
    }
  }
}


# 동그라미(아군, 적군) 하나 생성

generate_circle <- function(type) {
  list(
    x = runif(1, -game_size + circle_size, game_size - circle_size),
    y = runif(1, -game_size + circle_size, game_size - circle_size),
    type = type,         # "friend" 또는 "enemy"
    speed = move_range   # 초기 속도
  )
}


# 전체 동그라미 초기화

circles <- c(
  replicate(n_friends, generate_circle("friend"), simplify = FALSE),
  replicate(n_enemies, generate_circle("enemy"),  simplify = FALSE)
)


# 동그라미 이동 함수

move_circle <- function(circle) {
  if (circle$type == "enemy") {
    circle$speed <- circle$speed + enemy_speed_increase
  } else {
    circle$speed <- circle$speed + speed_increase
  }
  
  # 이동
  new_x <- circle$x + runif(1, -circle$speed, circle$speed)
  new_y <- circle$y + runif(1, -circle$speed, circle$speed)
  
  circle$x <- max(-game_size + circle_size, min(game_size - circle_size, new_x))
  circle$y <- max(-game_size + circle_size, min(game_size - circle_size, new_y))
  
  circle
}


# 충돌 판정 함수 (충돌 시 양쪽 동그라미 제거)

detect_collisions <- function(circles) {
  remaining_circles <- circles
  to_remove <- c()
  
  for (i in seq_along(circles)) {
    for (j in seq_along(circles)) {
      if (i < j) {
        dist_ij <- sqrt((circles[[i]]$x - circles[[j]]$x)^2 + 
                          (circles[[i]]$y - circles[[j]]$y)^2)
        if (!is.na(dist_ij) && dist_ij < 1.5 * circle_size) {
          
          # 충돌 시 사운드/메시지
          if (circles[[i]]$type == "friend" && circles[[j]]$type == "friend") {
            cat("사격금지! 아군이다!\n")
            beep(sound = 7)
          } else if (circles[[i]]$type == "enemy" && circles[[j]]$type == "enemy") {
            cat("뭐하는거야!\n")
            beep(sound = 11)
          } else {
            # 아군 vs 적군
            cat("으악!\n")
            beep(sound = 2)
          }
          to_remove <- c(to_remove, i, j)
        }
      }
    }
  }
  
  if (length(to_remove) > 0) {
    remaining_circles <- remaining_circles[-unique(to_remove)]
  }
  remaining_circles
}


# 게임 화면(플롯) 그리기 함수

plot_game <- function() {
  par(bg = "black")
  
  # 게임판 크기 설정
  plot(0, 0,
       xlim = c(-game_size * 1.5, game_size * 1.5),
       ylim = c(-game_size * 1.5, game_size * 1.5),
       type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
  
  # 게임판 테두리(흰색 사각형)
  lines(x = c(-game_size, game_size,  game_size, -game_size, -game_size),
        y = c(-game_size, -game_size, game_size,  game_size, -game_size),
        col = "white", lwd = 2)
  
  # 게임 제목
  title("전략적 우왕좌왕 전투", col.main = "white")
  


  rect_x_left  <- -game_size * 1.4
  rect_x_right <- -game_size * 1.2
  rect_y_bottom <- -0.5
  rect_y_top    <- 0.5
  
  rect(rect_x_left, rect_y_bottom, rect_x_right, rect_y_top,
       col = "green", border = "white", lwd = 3)
  text(x = (rect_x_left + rect_x_right)/2, y = 0, 
       labels = "나", col = "white")
  


  rect_x_left2  <-  game_size * 1.2
  rect_x_right2 <-  game_size * 1.4
  rect_y_bottom2 <- -0.5
  rect_y_top2    <- 0.5
  
  rect(rect_x_left2, rect_y_bottom2, rect_x_right2, rect_y_top2,
       col = "red", border = "white", lwd = 3)
  text(x = (rect_x_left2 + rect_x_right2)/2, y = 0,
       labels = "상대", col = "white")
  

  for (circle in circles) {
    points(circle$x, circle$y,
           col = ifelse(circle$type == "friend", "green", "red"),
           pch = 16, cex = 1.5)
  }
  
  n_friends_remaining <- sum(sapply(circles, function(c) c$type == "friend"))
  n_enemies_remaining <- sum(sapply(circles, function(c) c$type == "enemy"))
  
  legend("topright",
         legend = c(paste("아군:", n_friends_remaining),
                    paste("적군:", n_enemies_remaining)),
         col = c("green", "red"), pch = 16, bg = "black", text.col = "white")
}


# 게임 루프 (메인 로직)

game_loop <- function() {
  for (t in seq_len(game_time)) {
    Sys.sleep(1)  # 1초마다 업데이트
    
    # (전역) circles 갱신
    circles <<- lapply(circles, move_circle)
    circles <<- detect_collisions(circles)
    
    # 게임판 업데이트
    plot_game()
  }
  
  # 최종 결과 판정
  n_friends_remaining  <- sum(sapply(circles, function(c) c$type == "friend"))
  n_enemies_remaining <- sum(sapply(circles, function(c) c$type == "enemy"))
  
  if (n_friends_remaining > n_enemies_remaining) {
    # 아군 승리
    mtext(paste("게임 종료! 아군 승리! 남은 아군:", n_friends_remaining),
          side = 1, line = 3, col = "green")
    title("축하합니다! 승리했습니다!", line = -2, col.main = "green")
    beep(sound = 3)  # 승리 사운드
    
    # 폭죽 애니메이션 (sleep = 0.02 로 빠르게)
    Fireworks(num_fire   = 8,
              num_points = 300,
              steps      = 15,
              sleep      = 0.01,  # 더 빠르게
              color_alpha= 0.6,
              x_min      = -game_size*1.2, 
              x_max      =  game_size*1.2,
              y_min      = -game_size*1.2,
              y_max      =  game_size*1.2)
    
  } else if (n_friends_remaining < n_enemies_remaining) {
    # 적군 승리 -> 아군 패배
    mtext(paste("게임 종료! 적군 승리! 남은 적군:", n_enemies_remaining),
          side = 1, line = 3, col = "red")
    title("아쉽습니다. 패배하셨습니다!", line = -2, col.main = "red")
    
    # 아군 플레이어 쪽에 폭탄 연속 폭발 (sleep = 0.01 로 매우 빠르게)
    bomb_center_x <- -game_size * 1.3
    bomb_center_y <-  0
    BombExplosion(center_x = bomb_center_x,
                  center_y = bomb_center_y,
                  radius   = 0.3,
                  times    = 4,    # 연속 4번 폭발
                  num_points = 120,
                  steps    = 8,
                  sleep    = 0.01)  # 매우 빠르게
    
  } else {
    # 무승부
    mtext(paste("게임 종료! 무승부! 남은 아군:",
                n_friends_remaining, "남은 적군:", n_enemies_remaining),
          side = 1, line = 3, col = "yellow")
    title("무승부입니다!", line = -2, col.main = "yellow")
  }
}


# 실행

plot_game()

game_loop()
