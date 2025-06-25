# 모든 객체를 제거
rm(list = ls())

# 필요한 패키지 로드
library(png)
library(beepr)
library(extrafont)

my_clock_prince = function(duration = 60,         # 실행 시간(초). 기본값은 60초.
                           ctype = "both",        # 시계 유형("analogue" 또는 "both").
                           shandcol = "darkred",  # 초침 색상.
                           mhandcol = "darkgreen",# 분침 색상.
                           hhandcol = "darkblue", # 시침 색상.
                           clock_radius = 3,    # 시계 반지름
                           outer_radius = 6,      # 별의 바깥 반지름
                           inner_radius = 3)    # 별의 안쪽 반지름 (시계 반지름과 동일)
{
  
  # 이미지 읽기
  img <- readPNG("C:\\Users\\Yoo\\Desktop\\어린왕자.png")  ### 이미지 파일 경로 확인 필수!
  
  # 어린왕자 별의 하루는 지구 시간으로 약 0.558 시간 (24/43)
  earth_to_prince_ratio = 24 / 0.558  # 지구 시간 -> 어린왕자 시간 변환 비율
  
  cycles = 0  # 반복 횟수 초기화
  Sys.setenv(TZ='Asia/Seoul')  # 시간대를 대한민국으로 설정
  
  repeat {
    cycles = cycles + 1  # 반복 횟수 증가
    
    # 그래픽 창 설정
    plot.new()
    plot.window(xlim = c(-6, 6), ylim = c(-6, 6))
    par(usr = c(-6, 6, -6, 6))
    plot(NA, xlim = c(-6, 6), ylim = c(-6, 6), type = "n", axes = FALSE, xlab = "", ylab = "")
    rasterImage(img, -6, -6, 6, 6)
    
    # 시계 중심과 테두리 그리기
    points(0, 0, cex = 2, pch = 16, col = 2)  # 시계 중심 빨간 점
    
    # 별 모양의 좌표 정의 (5-pointed star)
    star_coords <- function(center = c(0,0), outer_radius = outer_radius, inner_radius = inner_radius, points = 5) {
      angles <- seq(0, 2*pi, length.out = points * 2 + 1)
      radii <- rep(c(outer_radius, inner_radius), times = points)
      coords <- cbind(
        center[1] + radii * sin(angles),
        center[2] + radii * cos(angles)
      )
      return(coords)
    }
    
    # 별 모양의 테두리 그리기
    star <- star_coords(center = c(0,0), outer_radius = outer_radius, inner_radius = inner_radius, points = 5)
    polygon(star, border = "gold", lwd = 8, col = NA)  # 별 테두리 그리기
    
    # 시계 숫자 대신 기호 배치
    symbols = c("☀", "☾", "★", "☁", "⚡", "☂", "✨", "❄", "⚙", "♠", "♣", "♦")
    t = seq(0, 330, by = 30)  # 각도 생성
    text(sin(t * pi / 180) * clock_radius, cos(t * pi / 180) * clock_radius, symbols, cex = 2, col = "black")  # 기호 사용
    
    # 현재 시간 가져오기 및 변환
    earth_time = Sys.time()
    lt = as.POSIXlt(earth_time)
    hr = lt$hour
    mi = lt$min
    se = as.numeric(format(earth_time, "%OS"))  # 초를 소수점까지 포함하여 가져오기
    
    # 지구 시간에서 어린왕자 별 시간으로 변환
    earth_seconds = hr * 3600 + mi * 60 + se
    prince_seconds = earth_seconds * earth_to_prince_ratio
    
    # 어린왕자 별 시간으로 시, 분, 초 계산
    total_prince_seconds = prince_seconds %% (24 * 3600)  # 24시간 주기
    prince_hr = floor(total_prince_seconds / 3600)        # 시
    remaining_seconds = total_prince_seconds %% 3600
    prince_mi = floor(remaining_seconds / 60)             # 분
    prince_se = remaining_seconds %% 60                   # 초 (소수점 포함)
    
    # 낮과 밤에 따른 배경색 결정 (6시 기준)
    if (prince_hr >= 6 && prince_hr < 18) {
      # 낮 시간: 연한 노란색 배경
      par(bg = "#FFFFCC")
      box_color = "#FFDAB9"  # 텍스트 배경 색상 (연한 노랑)
    } else {
      # 밤 시간: 연한 청록색 배경
      par(bg = "#CCFFFF")
      box_color = "#CCFFDD"  # 텍스트 배경 색상 (연한 청록)
    }
    
    # 각도 계산
    hr_angle = (prince_hr + prince_mi / 60 + prince_se / 3600) * 30  # 시침 각도
    mi_angle = (prince_mi + prince_se / 60) * 6                      # 분침 각도
    se_angle = prince_se * 6                                         # 초침 각도
    
    # 시계 손 그리기
    # 초침 그리기
    lines(c(0, sin(se_angle * pi / 180) * clock_radius * 0.8),
          c(0, cos(se_angle * pi / 180) * clock_radius * 0.8),
          col = shandcol, lwd = 2)
    
    # 분침 그리기
    lines(c(0, sin(mi_angle * pi / 180) * clock_radius * 0.7),
          c(0, cos(mi_angle * pi / 180) * clock_radius * 0.7),
          col = mhandcol, lwd = 3)
    
    # 시침 그리기
    lines(c(0, sin(hr_angle * pi / 180) * clock_radius * 0.6),
          c(0, cos(hr_angle * pi / 180) * clock_radius * 0.6),
          col = hhandcol, lwd = 4)
    
    # 스토리텔링 요소 및 알람 추가
    # 3시, 7시, 9시에 알람 발생
    
    if (prince_hr == 7) {
      # 오전 7시: 화산 청소하기
      rect(-3, -0.5, 3, 0.5, col = box_color, border = NA)  # 텍스트 배경 박스
      text(0, 0, "화산을 청소하는 중...", cex = 1, col = "darkorange", font=4)  # 텍스트 위치 조정
    }
    if (exists("previous_prince_hr")) {
      if (prince_hr == 7 && previous_prince_hr != 7) {
        for (i in 1:10) {  # 10번 알람
          beep()
          Sys.sleep(0.2)
        }
      }
      if (prince_hr == 9) {
        # 오전 9시: 바오밥나무 뿌리 제거
        rect(-3, -0.5, 3, 0.5, col = box_color, border = NA)  # 텍스트 배경 박스
        text(0, 0, "바오밥나무 뿌리 제거 중...", cex = 0.9, col = "darkgreen", font=4)
      }
      if (prince_hr == 9 && previous_prince_hr != 9) {
        for (i in 1:10) {  # 10번 알람
          beep()
          Sys.sleep(0.2)
        }
      }
      if (prince_hr == 15) {
        # 오후 3시: 장미에 물주기
        rect(-3, -0.5, 3, 0.5, col = box_color, border = NA)  # 텍스트 배경 박스
        text(0, 0, "장미에 물을 주는 중...", cex = 0.9, col = "red", font=4)  # 텍스트 위치 조정
        if (prince_hr == 15 && previous_prince_hr != 15) {
          for (i in 1:10) {  # 10번 알람
            beep()
            Sys.sleep(0.2)
          }
        }
      }
    }  
    # 이전 시간 저장
    previous_prince_hr = prince_hr
    
    # 디지털 시계 표시 (선택 사항)
    if(ctype == "both") {
      # 어린왕자 시간 표시
      mtext(sprintf("Prince Time: %02d:%02d:%05.2f", 
                    prince_hr, 
                    prince_mi, 
                    prince_se), 
            cex = 1.4, side = 1, line = 2, col = "black", family = "mono")
      
      # 청주 시간 표시
      mtext(sprintf("Cheongju Time: %02d:%02d:%05.2f", 
                    hr, 
                    mi, 
                    se),
            cex = 1, side = 1, line = 0.5, col = "black", family = "Times New Roman", font=2)
    }
    
    Sys.sleep(0.1)  # 0.1초 대기 후 업데이트
    if(duration == "unlimited")
      next
    else if(cycles > (duration * 10))  # 0.1초마다 업데이트하므로 cycles는 duration * 10 이상
      break
  }
}

# 시계 실행
my_clock_prince(duration = "unlimited", ctype = "both", clock_radius = 3.3, outer_radius = 6.6, inner_radius = 3.3)
