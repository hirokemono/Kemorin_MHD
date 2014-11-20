!
!
      module m_spectr_4_ispack
!
!   work array for FFT
!
      use m_precision
!
      implicit    none
!
      integer(kind=kint ) :: num_fft, num_io
      integer(kind=kint ) :: kx_max, ky_max, iz_max
      integer(kind=kint ) :: num_spectr
!
      real   (kind=kreal), allocatable  ::  phys_d(:)
      real   (kind=kreal), allocatable  ::  phys_io(:)

      character(len=kchara), allocatable :: fft_name(:)
      character(len=kchara), allocatable :: fft_comp(:)
      integer(kind=kint ), allocatable :: ifield_fft(:)
      integer(kind=kint ), allocatable :: icomp_fft(:)


      real   (kind=kreal), allocatable  ::  work(:)
!
      character(len=kchara  )               ::  spectr_data_head
      character(len=kchara  )               ::  spectr_data_name
!
      integer(kind=kint )               ::  spectr_data_code = 21
!
!        subroutine write_size_of_spectr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_spectr_name
!
!
        allocate( fft_name(num_fft) )
        allocate( fft_comp(num_fft) )
        allocate( ifield_fft(num_fft) )
        allocate( icomp_fft(num_fft) )
!
        ifield_fft = 0
        icomp_fft = 0
!
!
       end subroutine allocate_spectr_name
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_horiz_spectr
!
       num_spectr = kx_max*ky_max*iz_max
!
       allocate ( phys_d(num_spectr*num_fft) )
       allocate ( work(num_spectr*num_fft) )

       phys_d = 0.0d0
       work = 0.0d0
!
       end subroutine allocate_horiz_spectr
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_spectr_4_io
!
       num_io = kx_max*ky_max*iz_max
!
       allocate ( phys_io(num_io*num_fft) )
!
       phys_io = 0.0d0
!
       end subroutine allocate_spectr_4_io
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_spectr_name
!
!
        deallocate( fft_name )
        deallocate( fft_comp )
        deallocate( ifield_fft )
        deallocate( icomp_fft )
!
!
       end subroutine deallocate_spectr_name
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_horiz_spectr
!
       deallocate ( phys_d )
       deallocate ( work )
!
       end subroutine deallocate_horiz_spectr
!
!
!  ---------------------------------------------------------------------
!
       subroutine write_spectr_data(istep)
!
       use m_constants
       use set_spectr_file_name
!
       integer (kind = kint) :: istep
!
       integer (kind = kint) :: j
       integer (kind = kint) :: ix, iy, iz
       integer (kind = kint) :: ii1, ii2, ii3 , ii4
!
!
       call s_set_spectr_file_name(istep,spectr_data_name)
!
        write(*,*) 'merged spectral:     ', spectr_data_name
      open (spectr_data_code,  file=spectr_data_name, form='formatted', &
     &                        status ='unknown'                )

      write(spectr_data_code,*) 'number_of_results', num_fft
!
      do j = 1, num_fft
!
       write(spectr_data_code,*) trim( fft_name(j) )
       write(spectr_data_code,*) trim( fft_comp(j) )
!
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                phys_io(ii1), zero, zero, zero
        end do

       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*ix  )*iz_max + iz
         ii2   = (j-1)*num_spectr + (2*ix+1)*iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &               phys_io(ii1), phys_io(ii2), zero, zero
        end do
       end do
!
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                phys_io(ii1), zero, zero, zero
        end do

      do iy = 1, kx_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &            phys_io(ii1), zero, phys_io(ii3), zero
        end do
       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + (2*ix  )*iz_max + iz
         ii2   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + (2*ix+1)*iz_max + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + (2*ix  )*iz_max + iz
         ii4   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + (2*ix+1)*iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &             phys_io(ii1), phys_io(ii2),                          &
     &             phys_io(ii3), phys_io(ii4)
        end do
       end do

        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + iz_max + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &            phys_io(ii1), zero, phys_io(ii3), zero
        end do
      end do

        do iz = 1, iz_max
         ii1 = (j-1)*num_spectr + (iz_max*kx_max) + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                  phys_io(ii1), zero, zero, zero
        end do

       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1 = (j-1)*num_spectr + (iz_max*kx_max) + (2*ix  )*iz_max + iz
         ii2 = (j-1)*num_spectr + (iz_max*kx_max) + (2*ix+1)*iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                 phys_io(ii1), phys_io(ii2), zero, zero
        end do
       end do

        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (iz_max*kx_max) + iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                  phys_io(ii1), zero, zero, zero
        end do

      end do
!
      close(  spectr_data_code )
!
       end subroutine write_spectr_data

!
!  ---------------------------------------------------------------------
!
       subroutine read_number_of_field(istep)
!
       use set_spectr_file_name
!
       integer (kind = kint) :: istep
!
       character (len=kchara) :: tmpchara
!
      call s_set_spectr_file_name(istep, spectr_data_name)
      write(*,*) 'spectral data:     ', spectr_data_name
      open (spectr_data_code,  file=spectr_data_name, form='formatted', &
     &                        status ='unknown'                )

      read(spectr_data_code,*) tmpchara, num_fft
!
      close(spectr_data_code)
!
!
       end subroutine read_number_of_field
!
!  ---------------------------------------------------------------------
!
       subroutine read_spectr_data(istep)
!
       use set_spectr_file_name
!
       integer (kind = kint) :: istep
!
       integer (kind = kint) :: j
       integer (kind = kint) :: ix, iy, iz
       integer (kind = kint) :: ii1, ii2, ii3 , ii4
!
       real(kind = kreal)  :: rtmp
       character (len=kchara) :: tmpchara
!
!       write(*,*) kx_max, ky_max, iz_max
!
      call s_set_spectr_file_name(istep, spectr_data_name)
      write(*,*) 'spectral data:     ', spectr_data_name
      open (spectr_data_code,  file=spectr_data_name, form='formatted', &
     &                        status ='old'                )

      num_fft = 0
      read(spectr_data_code,*) tmpchara, num_fft
!
      write(*,*) 'num_fft', num_fft
      do j = 1, num_fft
       fft_name(j) = ''
       fft_comp(j) = ''
      end do
!
      do j = 1, num_fft
!
       read(spectr_data_code,*) fft_name(j)
       read(spectr_data_code,*) fft_comp(j)
!
       write(*,*) j, fft_name(j), fft_comp(j)
!
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + iz
         read(spectr_data_code,*) phys_io(ii1), rtmp, rtmp, rtmp
        end do
!
       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*ix  )*iz_max + iz
         ii2   = (j-1)*num_spectr + (2*ix+1)*iz_max + iz
         read(spectr_data_code,*)                                       &
     &               phys_io(ii1), phys_io(ii2), rtmp, rtmp
        end do
       end do
!
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + iz_max + iz
         read(spectr_data_code,*) phys_io(ii1), rtmp, rtmp, rtmp
        end do

      do iy = 1, kx_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + iz
         read(spectr_data_code,*)                                       &
     &            phys_io(ii1), rtmp, phys_io(ii3), rtmp
        end do
       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + (2*ix  )*iz_max + iz
         ii2   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + (2*ix+1)*iz_max + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + (2*ix  )*iz_max + iz
         ii4   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + (2*ix+1)*iz_max + iz
         read(spectr_data_code,*)                                       &
     &             phys_io(ii1), phys_io(ii2) ,                         &
     &             phys_io(ii3), phys_io(ii4)
        end do
       end do

        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (2*iy  )*(iz_max*kx_max)            &
     &          + iz_max + iz
         ii3   = (j-1)*num_spectr + (2*iy+1)*(iz_max*kx_max)            &
     &          + iz_max + iz
         read(spectr_data_code,*)                                       &
     &            phys_io(ii1), rtmp, phys_io(ii3), rtmp
        end do
      end do

        do iz = 1, iz_max
         ii1 = (j-1)*num_spectr + (iz_max*kx_max) + iz
         read(spectr_data_code,*)  phys_io(ii1), rtmp, rtmp, rtmp
        end do

       do ix = 1, ky_max/2-1
        do iz = 1, iz_max
         ii1 = (j-1)*num_spectr + (iz_max*kx_max) + (2*ix  )*iz_max + iz
         ii2 = (j-1)*num_spectr + (iz_max*kx_max) + (2*ix+1)*iz_max + iz
         read(spectr_data_code,*)                                       &
     &                 phys_io(ii1), phys_io(ii2), rtmp, rtmp
        end do
       end do

        do iz = 1, iz_max
         ii1   = (j-1)*num_spectr + (iz_max*kx_max) + iz_max + iz
         read(spectr_data_code,*)                                       &
     &                  phys_io(ii1), rtmp, rtmp, rtmp
        end do

      end do
!
      close(  spectr_data_code )
!
       end subroutine read_spectr_data
!
! -----------------------------------------------------------------------
!
      subroutine read_size_of_spectr
!
      use set_spectr_file_name
!
       real(kind = kreal) :: ztmp
       integer (kind = kint) :: ix, iy, iz
!
!    read outlines of spectr data
!
      open (spectr_data_code,  file=spec_mode_file_name,                &
     &                       form='formatted',  status ='unknown')
       
!
      kx_max = 0
      ky_max = 0
      iz_max = 0
      read(spectr_data_code,*) 
      do
       read(spectr_data_code,*, err = 99, end = 99) ix, iy, iz, ztmp
       kx_max = max(kx_max,ix)
       ky_max = max(ky_max,iy)
       iz_max = max(iz_max,iz)
      end do
  99  continue
!
      close(spectr_data_code)
!
      write(*,*) 'truncation level(x,y) and grid points (z)'
      write(*,*) kx_max, ky_max, iz_max
!
      kx_max = kx_max*2
      ky_max = ky_max*2
      num_spectr = kx_max * ky_max * iz_max
!
      return
      end subroutine read_size_of_spectr
!
!-----------------------------------------------------------------------
!
        subroutine write_size_of_spectr
!
       use m_geometry_data_4_merge
       use set_spectr_file_name
!
       integer(kind=kint ) :: ix,iy,iz,iii
!
       open (spectr_data_code,  file=spec_mode_file_name,               &
     &                       form='formatted', status ='unknown')
       
!
      write(spectr_data_code,*) ' kx, ky, z_id, z'
      do iy = 0, kx_max/2
        do ix = 0, ky_max/2
          do iz = 1, iz_max
            iii = iz*kx_max*ky_max
            write(spectr_data_code,'(3i16,1pe20.11)')                   &
     &            ix, iy, iz, merged%node%xx(iii,3)
        end do
       end do
      end do
!
      close(spectr_data_code)
!
      end subroutine write_size_of_spectr
!
! -----------------------------------------------------------------------
!
      end module m_spectr_4_ispack
