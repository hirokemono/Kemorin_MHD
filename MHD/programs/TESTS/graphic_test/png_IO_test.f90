!
      program png_header_test
!
      use ISO_C_BINDING
      use transfer_to_long_integers
      use byte_swap_f
      use t_buffer_4_gzip
!      use zlib_convert_text
!      use data_convert_by_zlib
      use gzip_infleate
      use write_bmp_image
      use gzip_defleate
!
!
      implicit none
!
!  ---------------------------------------------------------------------
!
!>        PNG header '\x89PNG\r\n\x1a\n'
      character(len=1), parameter :: PNG_HEADER(8)                      &
     &                    = (/char(137), 'P', 'N', 'G',                 &
     &                        char(13), char(10), char(26), char(10)/)
!
      type(buffer_4_gzip) :: zbuf
!
      integer(kind = 4), parameter :: id_png = 16
      integer(kind = 4), target :: int_ptr(1)
      character(len = 1), pointer :: cbuf(:)
!
      integer(kind = 4), parameter :: len_ihdr = 13
      character(len=4), parameter :: IHDR_HEADER = 'IHDR'
      integer(kind = 4), parameter :: ixpixel = 1600
      integer(kind = 4) :: iypixel = 1280
      character(len = 1), parameter :: BIT_DEPTH =           char(8)
      character(len = 1), parameter :: COLOR_TYPE =          char(2)
      character(len = 1), parameter :: COMPRESSION_METHOD =  char(0)
      character(len = 1), parameter :: FILTER_METHOD =       char(0)
      character(len = 1), parameter :: INTERLACE_METHOD =    char(0)
!
      integer(kind = 4), parameter :: len_gama = 4
      character(len=4), parameter :: GAMMA_HEADER = 'gAMA'
      real(kind = 8), parameter :: gamma = 1.0 / 2.2d0
      integer(kind = 4) :: int_gamma = int(gamma * 100000)
!
      integer(kind = 4), parameter :: len_time = 7
      character(len=4), parameter :: TIME_HEADER = 'tIME'
!
      integer(kind = 4), parameter :: len_text = 22
      character(len=4), parameter :: TEXT_HEADER = 'tEXt'
      character(len = 22), parameter                                    &
     &    :: SOFTWARE = "Software"//char(0)//"Calypso_viz"
!
      integer(kind = 4), parameter :: len_image = 4096
      character(len=4), parameter :: iDAT_HEADER = 'IDAT'
      character(len=3*ixpixel) :: image_line
!
      integer(kind = 4), parameter :: len_iend = 4
      character(len = 4), parameter :: iEND_HEADER = "IEND"
!
      integer(kind = 4) :: npix_x, npix_y
      integer(kind = 4) :: int_read(1)
      integer(kind = 4) :: i_crc, iy, ilength, icou, jcou, kcou, i, j
      integer(kind = 4) :: ntot_lenbuf, norg_lenbuf
      character(len = 17) :: chunk_buf13
!
      character(len = 1) :: ref_char, cmp_char
      character(len = 32767) :: readbuf
      character(len = 1), allocatable, target :: gzipbuf(:)
      character(len = 1), allocatable :: gzipbuf_tmp(:)
      character(len = 1), allocatable, target :: chara_dat(:)
!
      integer(kind = kint), allocatable :: i_rgb_line(:,:)
      integer(kind = kint), allocatable :: i_rgb_up(:,:)
      character(len = 1), allocatable :: rgb(:)
      character(len = kchara) :: fhead
!
      integer(kind = kint) :: iflag_filter, ntot_png_img
!
      logical :: flag_little_endian = .TRUE.
!
      integer date_time(8)
      character(len=1) :: dt_c(30)
      call date_and_time(dt_c(1), dt_c(11), dt_c(21), date_time)
!
       write(*,*) 'date_time    array values:'
       write(*,*) 'year=',date_time(1)
       write(*,*) 'month_of_year=',date_time(2)
       write(*,*) 'day_of_month=',date_time(3)
       write(*,*) 'time difference in minutes=',date_time(4)
       write(*,*) 'hour of day=',date_time(5)
       write(*,*) 'minutes of hour=',date_time(6)
       write(*,*) 'seconds of minute=',date_time(7)
       write(*,*) 'milliseconds of second=',date_time(8)
       write(*,*)  'DATE=',dt_c( 1:10)
       write(*,*)  'TIME=',dt_c(11:20)
       write(*,*)  'ZONE=',dt_c(21:30)
!

      i_crc = 0
      call crc32_4_png(len(iEND_HEADER), iEND_HEADER, i_crc)
      write(*,'(a, i16, Z12)') 'crc32_4_png(ilength, cbuf)',   &
     &      i_crc, i_crc
!
      i_crc = 0
      call crc32_4_png(1, iEND_HEADER(1:1), i_crc)
      call crc32_4_png(1, iEND_HEADER(2:2), i_crc)
      call crc32_4_png(1, iEND_HEADER(3:3), i_crc)
      call crc32_4_png(1, iEND_HEADER(4:4), i_crc)
      write(*,'(a, i16, Z12)') 'crc32_4_png(ilength, cbuf)',   &
     &      i_crc, i_crc
!
!
!
!      open(id_png, file='tako3.png', STATUS = 'old', access='STREAM')
      open(id_png, file='pvr_magne.5000.png', STATUS = 'old', access='STREAM')
      read(id_png) readbuf(1:8)
      do iy = 1, size(PNG_HEADER)
        cmp_char = readbuf(iy:iy)
        if(cmp_char .ne. PNG_HEADER(iy)) write(*,*) 'Fail header at ', iy
      end do
!
      i_crc = 0
      read(id_png) int_read
      if(int_read(1) .eq. len_ihdr) then
        flag_little_endian = .FALSE.
        write(*,*) 'Keep big endian flag correctly'
      else
        flag_little_endian = .TRUE.
        call byte_swap_int4_f(cast_long(1), int_read)
        if((int_read(1) .eq. len_ihdr) .and. flag_little_endian)   &
     &       write(*,*) 'Flip endian flag correctly'
      end if
!
      i_crc = 0
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(IHDR_HEADER)
        if(readbuf(iy:iy) .ne. IHDR_HEADER(iy:iy)) write(*,*) 'Fail at ', iy
      end do
!      
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      call crc32_4_png(len(num2bit4_big(int_read(1))), num2bit4_big(int_read(1)), i_crc)
      npix_x = int_read(1)
      write(*,*) 'x_pix', npix_x
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      call crc32_4_png(len(num2bit4_big(int_read(1))), num2bit4_big(int_read(1)), i_crc)
      npix_y = int_read(1)
      write(*,*) 'y_pix', npix_y
!
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. BIT_DEPTH) write(*,*) 'Fail at BIT_DEPTH'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. COLOR_TYPE) write(*,*) 'Fail at COLOR_TYPE'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. COMPRESSION_METHOD) write(*,*) 'Fail at COMPRESSION_METHOD'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. FILTER_METHOD) write(*,*) 'Fail at FILTER_METHOD'
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      if(readbuf(1:1) .ne. INTERLACE_METHOD) write(*,*) 'Fail at INTERLACE_METHOD'
!
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
      write(*,*) 'crc32', int_read(1), i_crc
!
!
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. len_gama) write(*,*) 'Fail at len_gama'
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(GAMMA_HEADER)
        if(readbuf(iy:iy) .ne. GAMMA_HEADER(iy:iy)) write(*,*) 'Fail at ', iy
      end do
!
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read(1))
      call crc32_4_png(len(num2bit4_big(int_read(1))), num2bit4_big(int_read(1)), i_crc)
      if(int_read(1) .ne. int_gamma) write(*,*) 'Fail at int_gamma'
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
      write(*,*) 'crc32', int_read(1), i_crc
!
!
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. len_time) write(*,*) 'Fail at len_time'
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(TIME_HEADER)
        if(readbuf(iy:iy) .ne. TIME_HEADER(iy:iy)) write(*,*) 'Fail at ', iy
      end do
!
      read(id_png) readbuf(1:2)
      call crc32_4_png(len(readbuf(1:2)), readbuf(1:2), i_crc)
      write(*,*) 'year:   ', 256*iachar(readbuf(1:1)) + iachar(readbuf(2:2))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Month:   ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Day:     ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Hour:    ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Minuts:  ', iachar(readbuf(1:1))
      read(id_png) readbuf(1:1)
      call crc32_4_png(len(readbuf(1:1)), readbuf(1:1), i_crc)
      write(*,*) 'Seconds: ', iachar(readbuf(1:1))
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
      write(*,*) 'crc32', int_read(1), i_crc
!
      i_crc = 0
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      ilength = int_read(1)
      read(id_png) readbuf(1:4)
      call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
      do iy = 1, len(TEXT_HEADER)
        if(readbuf(iy:iy) .ne. TEXT_HEADER(iy:iy)) write(*,*) 'Fail at ', iy
      end do
      read(id_png) readbuf(1:ilength)
      call crc32_4_png(len(readbuf(1:ilength)), readbuf(1:ilength), i_crc)
      write(*,*) 'Text ', readbuf(1:ilength)
      read(id_png) int_read(1:1)
      if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
      if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
      write(*,*) 'crc32', int_read(1), i_crc
!
      icou = 0
      ntot_lenbuf = 0
      allocate(gzipbuf(ntot_lenbuf))
      do
        icou = icou + 1
        i_crc = 0
        read(id_png) int_read(1:1)
        if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
        ilength = int_read(1)
!
        read(id_png) readbuf(1:4)
        call crc32_4_png(len(readbuf(1:4)), readbuf(1:4), i_crc)
!
        if(readbuf(1:4) .eq. iEND_HEADER) then
          write(*,*) icou, 'exit loop'
          exit
        else if(readbuf(1:4) .eq. iDAT_HEADER) then
          write(*,*) icou, 'read data', ilength, ' byte'
        else
          write(*,*) icou, 'Something wrong'
          exit
        end if
!
        read(id_png) readbuf(1:ilength)
        call crc32_4_png(ilength, readbuf(1:ilength), i_crc)
!
        read(id_png) int_read(1:1)
        if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
        if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
!        write(*,*) 'crc32', int_read(1), i_crc
!
        allocate(gzipbuf_tmp(ntot_lenbuf))
        if(ntot_lenbuf .gt. 0) gzipbuf_tmp(1:ntot_lenbuf) = gzipbuf(1:ntot_lenbuf)
        deallocate(gzipbuf)
!
        norg_lenbuf = ntot_lenbuf
        ntot_lenbuf = ntot_lenbuf + ilength
        allocate(gzipbuf(ntot_lenbuf))
        gzipbuf(1:norg_lenbuf) = gzipbuf_tmp(1:norg_lenbuf)
        deallocate(gzipbuf_tmp)
!
        do iy = 1, ilength
          gzipbuf(iy+norg_lenbuf) = readbuf(iy:iy)
        end do
      end do
!
        read(id_png) int_read(1:1)
        if(flag_little_endian) call byte_swap_int4_f(cast_long(1), int_read)
        if(int_read(1) .ne. i_crc) write(*,*) 'Fail at CRC'
        write(*,*) 'crc32', int_read(1), i_crc
      close (id_png)
!
      allocate(chara_dat(3*(npix_x+1)*npix_y))
      zbuf%ilen_gz = ntot_lenbuf
      call alloc_zip_buffer(zbuf)
      call zlib_infleat_char_once(ntot_lenbuf, gzipbuf,                 &
     &                          (3*(npix_x+1)*npix_y), chara_dat, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      allocate(rgb(3*npix_x*npix_y))
      call unfilter_png_rgb(npix_x, npix_y, chara_dat, rgb)
      deallocate(chara_dat)
!
      fhead = 'tako6'
      call pixout_BMP(fhead, npix_x, npix_y, rgb)
!
!
!
!      int_ptr(1) = len_ihdr
!      call c_f_pointer(C_LOC(int_ptr), cbuf, [4])
!      write(*,*) int(cbuf(1:4))
!
      open(id_png, file='tako3.png', access='STREAM')
      write(id_png) PNG_HEADER(1:8)
!
      i_crc = 0
      write(id_png) num2bit4_big(len_ihdr)
      write(id_png) IHDR_HEADER
      call crc32_4_png(len(IHDR_HEADER), IHDR_HEADER, i_crc)
      write(id_png) num2bit4_big(npix_x)
      call crc32_4_png(4, num2bit4_big(npix_x), i_crc)
      write(id_png) num2bit4_big(npix_y)
      call crc32_4_png(4, num2bit4_big(npix_y), i_crc)
      write(id_png) BIT_DEPTH
      call crc32_4_png(len(BIT_DEPTH), BIT_DEPTH, i_crc)
      write(id_png) COLOR_TYPE
      call crc32_4_png(len(COLOR_TYPE), COLOR_TYPE, i_crc)
      write(id_png) COMPRESSION_METHOD
      call crc32_4_png(len(COMPRESSION_METHOD), COMPRESSION_METHOD, i_crc)
      write(id_png) FILTER_METHOD
      call crc32_4_png(len(FILTER_METHOD), FILTER_METHOD, i_crc)
      write(id_png) INTERLACE_METHOD
      call crc32_4_png(len(INTERLACE_METHOD), INTERLACE_METHOD, i_crc)
      write(id_png) num2bit4_big(i_crc)
!
!
      i_crc = 0
      write(id_png) num2bit4_big(len_gama)
      write(id_png) GAMMA_HEADER
      call crc32_4_png(len(GAMMA_HEADER), GAMMA_HEADER, i_crc)
      write(id_png) num2bit4_big(int_gamma)
      call crc32_4_png(len(num2bit4_big(int_gamma)), num2bit4_big(int_gamma), i_crc)
      write(id_png) num2bit4_big(i_crc)
!
!
      i_crc = 0
      write(id_png) num2bit4_big(len_time)
      write(id_png) TIME_HEADER
      call crc32_4_png(len(TIME_HEADER), TIME_HEADER, i_crc)
      write(id_png) num2bit2_big(date_time(1))
      call crc32_4_png(len(num2bit2_big(date_time(1))),             &
     &    num2bit2_big(date_time(1)), i_crc)
      write(id_png) char(date_time(2))
      call crc32_4_png(1, char(date_time(2)), i_crc)
      write(id_png) char(date_time(3))
      call crc32_4_png(1, char(date_time(3)), i_crc)
      write(id_png) char(date_time(5))
      call crc32_4_png(1, char(date_time(5)), i_crc)
      write(id_png) char(date_time(6))
      call crc32_4_png(1, char(date_time(6)), i_crc)
      write(id_png) char(date_time(7))
      call crc32_4_png(1, char(date_time(7)), i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      i_crc = 0
      write(id_png) num2bit4_big(len_text)
      write(id_png) TEXT_HEADER
      call crc32_4_png(len(TEXT_HEADER), TEXT_HEADER, i_crc)
      write(id_png) SOFTWARE
      call crc32_4_png(len(SOFTWARE), SOFTWARE, i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      ntot_png_img = 3*(npix_x+1)*npix_y
      zbuf%ilen_gz = int(real(ntot_png_img)*1.01+24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      allocate(chara_dat(ntot_png_img))
      do j = 1, npix_y
        icou = 1 + (j-1)*(3*npix_x+1)
        chara_dat(icou) = char(0)
        do i = 1, npix_x
          icou = 1 + 3*(i-1) + (j-1)*(3*npix_x+1)
          jcou =     3*(i-1) + (npix_y-j)*(3*npix_x)
          chara_dat(icou+1:icou+3) = rgb(jcou+1:jcou+3)
        end do
      end do
!
      zbuf%ilen_gzipped = 0
      call zlib_defleat_char_once(ntot_png_img, chara_dat,              &
     &     int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      write(*,*) j, int(zbuf%ilen_gzipped), ntot_png_img
!
      i_crc = 0
      write(id_png) num2bit4_big(int(zbuf%ilen_gzipped))
      write(id_png) iDAT_HEADER
      call crc32_4_png(len(iDAT_HEADER), iDAT_HEADER, i_crc)
      write(id_png) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call crc32_4_png(int(zbuf%ilen_gzipped), zbuf%gzip_buf(1), i_crc)
      write(id_png) num2bit4_big(i_crc)
      call dealloc_zip_buffer(zbuf)
!
!
!
!      zbuf%ilen_gz = int(real(3*(npix_x+1))*1.01+24,KIND(zbuf%ilen_gz))
!      call alloc_zip_buffer(zbuf)
!      allocate(chara_dat(3*npix_x+1))
!      do j = 1, npix_y
!        chara_dat(0) = char(0)
!        do i = 1, npix_x
!          jcou =     3*(i-1) + (j-1)*(3*npix_x)
!          chara_dat(3*i-1:3*i+1) = rgb(jcou+1:jcou+3)
!        end do
!
!        zbuf%ilen_gzipped = 0
!        call zlib_defleat_char_once((3*npix_x+1), chara_dat,           &
!     &     int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!        write(*,*) j, int(zbuf%ilen_gzipped), 3*npix_x+1
!
!        i_crc = 0
!        write(id_png) num2bit4_big(int(zbuf%ilen_gzipped))
!        write(id_png) iDAT_HEADER
!        call crc32_4_png(len(iDAT_HEADER), iDAT_HEADER, i_crc)
!        write(id_png) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
!        call crc32_4_png(int(zbuf%ilen_gzipped), zbuf%gzip_buf(1), i_crc)
!        write(id_png) num2bit4_big(i_crc)
!      end do
!        call dealloc_zip_buffer(zbuf)
      deallocate(rgb)
!
      deallocate(chara_dat)
!
!      do iy = 1, npix_y
!        i_crc = 0
!        write(id_png) num2bit4_big(len_image)
!        write(id_png) iDAT_HEADER
!        call crc32_4_png(len(iDAT_HEADER), iDAT_HEADER, i_crc)
!
!        call defleate_characters(len(image_line), image_line, zbuf)
!        write(id_png) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
!        call crc32_4_png(int(zbuf%ilen_gzipped), zbuf%gzip_buf(1), i_crc)
!        call dealloc_zip_buffer(zbuf)
!        write(id_png) num2bit4_big(i_crc)
!      end do
!
      i_crc = 0
      write(id_png) num2bit4_big(len_iend)
      write(id_png) iEND_HEADER
      call crc32_4_png(len(iEND_HEADER), iEND_HEADER, i_crc)
      write(id_png) num2bit4_big(i_crc)
!
      close(id_png)
!
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
       character(len=4) function num2bit4_big(inum)
!
       integer, intent(in) :: inum
!
       integer :: itmp0, itmp1, itmp2
!
!
       if(inum .lt. 0) then
         itmp1 = inum + 2147483647 + 1
         itmp2 = itmp1 / 256**3
         num2bit4_big(1:1) = char(itmp2+128)
       else
         itmp1 = inum
         itmp2 = itmp1 / 256**3
         num2bit4_big(1:1) = char(itmp2)
       end if
!
       itmp1 =-itmp2 * 256**3 +itmp1
       itmp2 = itmp1 / 256**2
       num2bit4_big(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256**2 +itmp1
       itmp2 = itmp1 / 256
       num2bit4_big(3:3) = char(itmp2)
       itmp1 =-itmp2 * 256    +itmp1
       num2bit4_big(4:4) = char(itmp1)
!
       end function num2bit4_big
!
!------------------------------------------------------------------------
!
       character(len=2) function num2bit2_big(inum)
!
       integer, intent(in) :: inum
!
       integer :: itmp0, itmp1, itmp2
!
!
       if(inum .lt. 0) then
         itmp1 = inum + 32767 + 1
         itmp2 = itmp1 / 256
         num2bit2_big(1:1) = char(itmp2+128)
       else
         itmp1 = inum
         itmp2 = itmp1 / 256
         num2bit2_big(1:1) = char(itmp2)
       end if
!
       itmp1 =-itmp2 * 256 +itmp1
       num2bit2_big(2:2) = char(itmp1)
!
       end function num2bit2_big
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine crc32_4_png(ilength, cbuf, i_crc)
!
      integer(kind = 4), intent(in) :: ilength
      character(len = 1), intent(in) :: cbuf(ilength)
      integer(kind = 4), intent(inout) :: i_crc
!
!      integer(kind = 4), parameter  :: crc_init =  Z'FFFFFFFF'
      integer(kind = 4), parameter  :: crc_init =  -1
!      integer(kind = 8), parameter  :: crc_init =  4294967295
!      integer(kind = 8), parameter  :: crc_init =  3 * 1431655765
!      integer(kind = 4), parameter  :: crc_magic = Z'EDB88320'
      integer(kind = 4), parameter  :: crc_magic = -306674912
!      integer(kind = 8), parameter  :: crc_magic = 3988292384
!      integer(kind = 8), parameter  :: crc_magic = 2 * 1994146192
      integer(kind = 4) :: i_table(0:255)
!
      integer(kind = 4) :: i_tmp, i_b
      integer(kind = 4) :: i1, i3
      integer(kind = 4) :: i, j
!
!
      do i = 0, 255
        i_tmp = i
        do j = 0, 7
          i_b = iand(i_tmp,1)
!          write(*,*) i,j,i_b, i_tmp
          i_tmp = ishft(i_tmp,-1)
          if(i_b .gt. 0) i_tmp = ieor(i_tmp,crc_magic)
        end do
        i_table(i) = i_tmp
      end do
!
      i_crc = not(i_crc)
      do i = 1, ilength
        i1 = iachar(cbuf(i))
        i3 = iand(ieor(i_crc,i1),255)
        i_crc = ieor(i_table(i3), ishft(i_crc,-8))
!        write(*,*) i, i_crc
      end do
      i_crc = not(i_crc)
!      write(*,*) crc32_4_png
!
      end subroutine crc32_4_png
!
!------------------------------------------------------------------------
!
      end program png_header_test
!
!------------------------------------------------------------------------
!
!
!------------------------------------------------------------------------
!
      subroutine unfilter_png_rgb                                       &
     &         (npix_x, npix_y, png_rgb, rgb)
!
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x, npix_y
      character(len = 1), intent(in) :: png_rgb(3*(npix_x+1)*npix_y)
      character(len = 1), intent(inout) :: rgb(3*npix_x*npix_y)
!
      integer(kind = 4), allocatable :: i_rgb_line(:,:)
      integer(kind = 4), allocatable :: i_rgb_up(:,:)
!
      integer(kind = 4) :: iflag_filter
      integer(kind = 4) :: i, j, icou, jcou
!
!
      allocate(i_rgb_line(4,0:npix_x))
      allocate(i_rgb_up(4,0:npix_x))
!$omp parallel workshare
      i_rgb_line(1:4,0:npix_x) = 0
      i_rgb_up(1:4,0:npix_x) = 0
!$omp end parallel workshare
!
      do j = 1, npix_y
        iflag_filter = iachar(png_rgb(1+(j-1)*(3*npix_x+1)))
!        write(*,*) j, iflag_filter
!
!$omp parallel workshare
        i_rgb_up(1:4,0:npix_x) =   i_rgb_line(1:4,0:npix_x)
!$omp end parallel workshare
        i_rgb_line(1:4,0) = 0
!$omp parallel do private(i,icou)
        do i = 1, npix_x
          icou = 1 + 3*(i-1) + (j-1)*(3*npix_x+1)
          i_rgb_line(1:3,i) = iachar(png_rgb(icou+1:icou+3))
        end do
!$omp end parallel do
!
        call  unfilter_png_rgba_line(iflag_filter, 1, j,                &
     &                               npix_x, i_rgb_up, i_rgb_line)
!
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = 3*(i-1) + (npix_y-j)*(3*npix_x)
          rgb(jcou+1:jcou+3) = char(i_rgb_line(1:3,i))
        end do
!$omp end parallel do
      end do
      deallocate(i_rgb_line, i_rgb_up)
!
      end subroutine unfilter_png_rgb
!
!------------------------------------------------------------------------
!
      subroutine set_paeth_predictor(ileft, iup, idiag, ipaeth)
!
      implicit none
!
      integer(kind = 4), intent(in) :: ileft(4), iup(4), idiag(4)
      integer(kind = 4), intent(inout) :: ipaeth(4)
!
      integer(kind = 4) :: itmp(4), jleft(4), jup(4), jdiag(4)
      integer(kind = 4) :: nd
!
      itmp(1:4) = ileft(1:4) + iup(1:4) - idiag(1:4)
!
      jleft(1:4) = abs(itmp(1:4) - ileft(1:4))
      jup(1:4) =   abs(itmp(1:4) - iup(1:4))
      jdiag(1:4) = abs(itmp(1:4) - idiag(1:4))
!
      do nd = 1, 3
        if(jleft(nd) .le. jup(nd) .and. jleft(nd) .le. jdiag(nd)) then
          ipaeth(nd) = ileft(nd)
        else if(jup(nd) .le. jdiag(nd)) then
          ipaeth(nd) = iup(nd)
        else
          ipaeth(nd) = idiag(nd)
        end if
      end do
!
      end subroutine set_paeth_predictor
!
!------------------------------------------------------------------------
!
      subroutine unfilter_png_rgba_line                                 &
     &         (iflag_filter, iflag_left_edge, iflag_bottom,            &
     &          npix_x, i_rgb_up, i_rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: iflag_filter
      integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgb_up(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
      if(iflag_filter .eq. 1) then
        do i = 1, npix_x
          i_rgb_line(1:4,i)                                           &
     &         = mod((i_rgb_line(1:4,i)+i_rgb_line(1:4,i-1)+256),256)
        end do
      else if(iflag_filter .eq. 2) then
        do i = 1, npix_x
          i_rgb_line(1:4,i)                                           &
     &         = mod((i_rgb_line(1:4,i)+i_rgb_up(1:4,i)+256),256)
        end do
!
      else if(iflag_filter .eq. 3) then
        do i = 1, npix_x
          if(iflag_left_edge .eq. 1 .and. iflag_bottom .eq. 1           &
     &       .and. i .eq. 1) then
            ipaeth(1:4) = 0
          else if(i .eq. 1) then
            ipaeth(1:4) = i_rgb_up(1:4,i)
          else if(iflag_bottom .eq. 1) then
            ipaeth(1:4) = i_rgb_line(1:4,i-1)
          else
            ipaeth(1:4) = (i_rgb_line(1:4,i-1) + i_rgb_up(1:4,i)) / 2
          end if
          i_rgb_line(1:4,i)                                             &
     &             = mod((i_rgb_line(1:4,i)+ipaeth(1:4)+256),256)
        end do
!
      else if(iflag_filter .eq. 4) then
        do i = 1, npix_x
          call set_paeth_predictor                                      &
     &       (i_rgb_line(1,i-1), i_rgb_up(1,i), i_rgb_up(1,i-1),        &
     &        ipaeth)
          i_rgb_line(1:4,i)                                             &
     &             = mod((i_rgb_line(1:4,i)+ipaeth(1:4)+256),256)
        end do 
      end if
!
      end subroutine unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine non_filter_png_rgb_line(npix_x, i_rgb_line, rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:3*npix_x)
!
      integer(kind = 4) :: i
!
      rgb_line(0) = char(0)
!$omp parallel do
      do i = 1, npix_x
        rgb_line(3*i-2:3*i) = char(i_rgb_line(1:3,i))
      end do
!$omp end parallel do
!
      end subroutine non_filter_png_rgb_line
!
!------------------------------------------------------------------------
!
      subroutine sub_filter_png_rgb_line(npix_x, i_rgb_line, rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:3*npix_x)
!
      integer(kind = 4) :: i
!
      rgb_line(0) = char(1)
      do i = 1, npix_x
        i_rgb_line(1:4,i)                                               &
     &       = mod((i_rgb_line(1:4,i)-i_rgb_line(1:4,i-1)+256),256)
        rgb_line(3*i-2:3*i) = char(i_rgb_line(1:3,i))
      end do
!
      end subroutine sub_filter_png_rgb_line
!
!------------------------------------------------------------------------
!
      subroutine up_filter_png_rgb_line                                 &
     &         (npix_x, i_rgb_up, i_rgb_line, rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgb_up(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:3*npix_x)
!
      integer(kind = 4) :: i
!
      rgb_line(0) = char(2)
      do i = 1, npix_x
        i_rgb_line(1:4,i)                                               &
     &       = mod((i_rgb_line(1:4,i)-i_rgb_up(1:4,i)+256),256)
        rgb_line(3*i-2:3*i) = char(i_rgb_line(1:3,i))
      end do
!
      end subroutine up_filter_png_rgb_line
!
!------------------------------------------------------------------------
!
      subroutine ave_filter_png_rgb_line                                &
     &         (iflag_left_edge, iflag_bottom,                          &
     &          npix_x, i_rgb_up, i_rgb_line, rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgb_up(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:3*npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
      rgb_line(0) = char(3)
      do i = 1, npix_x
        if(iflag_left_edge .eq. 1 .and. iflag_bottom .eq. 1             &
     &     .and. i .eq. 1) then
          ipaeth(1:4) = 0
        else if(i .eq. 1) then
          ipaeth(1:4) = i_rgb_up(1:4,i)
        else if(iflag_bottom .eq. 1) then
          ipaeth(1:4) = i_rgb_line(1:4,i-1)
        else
          ipaeth(1:4) = (i_rgb_line(1:4,i-1) + i_rgb_up(1:4,i)) / 2
        end if
        i_rgb_line(1:4,i)                                               &
     &           = mod((i_rgb_line(1:4,i)-ipaeth(1:4)+256),256)
        rgb_line(3*i-2:3*i) = char(i_rgb_line(1:3,i))
      end do
!
      end subroutine ave_filter_png_rgb_line
!
!------------------------------------------------------------------------
!
      subroutine paeth_filter_png_rgb_line                              &
     &         (npix_x, i_rgb_up, i_rgb_line, rgb_line)
!
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgb_up(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:3*npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
      rgb_line(0) = char(4)
      do i = 1, npix_x
        call set_paeth_predictor                                        &
     &     (i_rgb_line(1,i-1), i_rgb_up(1,i), i_rgb_up(1,i-1),          &
     &      ipaeth)
        i_rgb_line(1:4,i)                                               &
     &           = mod((i_rgb_line(1:4,i)+ipaeth(1:4)+256),256)
        rgb_line(3*i-2:3*i) = char(i_rgb_line(1:3,i))
      end do 
!
      end subroutine paeth_filter_png_rgb_line
!
!------------------------------------------------------------------------
!
      subroutine sel_filter_png_rgb                                     &
     &         (npix_x, npix_y, rgb, png_rgb)
!
      use t_buffer_4_gzip
      use gzip_defleate
      use transfer_to_long_integers
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x, npix_y
      character(len = 1), intent(in) :: rgb(3*npix_x*npix_y)
      character(len = 1), intent(inout) :: png_rgb(3*(npix_x+1)*npix_y)
!
      type(buffer_4_gzip) :: zbuf
!
      character(len = 1) :: rgb_line(0:3*npix_x)
      integer(kind = 4), allocatable :: i_rgb_line(:,:,:)
      integer(kind = 4), allocatable :: i_rgb_up(:,:)
!
      integer(kind = 4) :: iflag_filter, iflag_sel
      integer(kind = 4) :: i, j, k, icou, jcou
      integer :: len_gzipped
!
!
      allocate(i_rgb_line(4,0:npix_x,0:4))
      allocate(i_rgb_up(4,0:npix_x))
!$omp parallel workshare
      i_rgb_line(1:4,0:npix_x,0:4) = 0
      i_rgb_up(1:4,0:npix_x) = 0
!$omp end parallel workshare
!
      iflag_sel = 0
      do j = 1, npix_y
!$omp parallel workshare
        i_rgb_up(1:4,0:npix_x) = i_rgb_line(1:4,0:npix_x,iflag_sel)
!$omp end parallel workshare
        i_rgb_line(1:4,0,0) = 0
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = 3*(i-1) + (npix_y-j)*(3*npix_x)
          i_rgb_line(1:3,i,0) = iachar(rgb(jcou+1:jcou+3))
        end do
!$omp end parallel do
!
        do k = 1, 4
!$omp parallel workshare
          i_rgb_line(1:4,0:npix_x,k) = i_rgb_line(1:4,0:npix_x,1)
!$omp end parallel workshare
        end do
!
        zbuf%ilen_gz = int(real(3*npix_x+1)*1.01+24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
!
        call non_filter_png_rgb_line(npix_x, i_rgb_line(1,0,0), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!
        call sub_filter_png_rgb_line(npix_x, i_rgb_line(1,0,1), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!
        call  up_filter_png_rgb_line(npix_x, i_rgb_up,                  &
     &                               i_rgb_line(1,0,2), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!
        call  ave_filter_png_rgb_line(1, j, npix_x, i_rgb_up,           &
     &                                i_rgb_line(1,0,3), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!
        call  paeth_filter_png_rgb_line(npix_x, i_rgb_up,               &
     &                                  i_rgb_line(1,0,4), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
!
!        len_gzipped = zbuf(iflag_sel)%ilen_gzipped
!
!$omp parallel do private(i,icou)
        do i = 1, npix_x
          icou = 1 + 3*(i-1) + (j-1)*(3*npix_x+1)
          png_rgb(icou+1:icou+3) = char(i_rgb_line(1:3,i,iflag_sel))
        end do
!$omp end parallel do
      end do
      deallocate(i_rgb_line, i_rgb_up)
!
      end subroutine sel_filter_png_rgb
!
!------------------------------------------------------------------------
!
      subroutine non_filter_png_rgb(npix_x, npix_y, rgb, png_rgb)
!
      use t_buffer_4_gzip
      use gzip_defleate
      use transfer_to_long_integers
      implicit none
!
      integer(kind = 4), intent(in) :: npix_x, npix_y
      character(len = 1), intent(in) :: rgb(3*npix_x*npix_y)
      character(len = 1), intent(inout) :: png_rgb(3*(npix_x+1)*npix_y)
!
      type(buffer_4_gzip) :: zbuf
!
      character(len = 1) :: rgb_line(0:3*npix_x)
      integer(kind = 4), allocatable :: i_rgb_line(:,:)
      integer(kind = 4), allocatable :: i_rgb_up(:,:)
!
      integer(kind = 4) :: iflag_filter, iflag_sel
      integer(kind = 4) :: i, j, k, icou, jcou
!
!
      allocate(i_rgb_line(4,0:npix_x))
      allocate(i_rgb_up(4,0:npix_x))
!$omp parallel workshare
      i_rgb_line(1:4,0:npix_x) = 0
      i_rgb_up(1:4,0:npix_x) = 0
!$omp end parallel workshare
!
      iflag_sel = 0
      do j = 1, npix_y
!$omp parallel workshare
        i_rgb_up(1:4,0:npix_x) = i_rgb_line(1:4,0:npix_x)
!$omp end parallel workshare
        i_rgb_line(1:4,0) = 0
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = 3*(i-1) + (npix_y-j)*(3*npix_x)
          i_rgb_line(1:3,i) = iachar(rgb(jcou+1:jcou+3))
        end do
!$omp end parallel do
!
        zbuf%ilen_gz = int(real(3*npix_x+1)*1.01+24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
!
        call non_filter_png_rgb_line(npix_x, i_rgb_line(1,0), rgb_line)
        call zlib_defleat_char_once((3*(npix_x+1)), rgb_line(0),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      end do
      deallocate(i_rgb_line, i_rgb_up)
!
      end subroutine non_filter_png_rgb
!
!------------------------------------------------------------------------
!
