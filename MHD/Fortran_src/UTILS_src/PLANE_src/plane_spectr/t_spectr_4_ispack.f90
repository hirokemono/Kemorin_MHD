!
!
!   work array for FFT
!
!!      subroutine alloc_spectr_name(plane_fft_wk)
!!      subroutine alloc_horiz_spectr(plane_fft_wk)
!!      subroutine alloc_spectr_4_io(plane_fft_wk)
!!      subroutine dealloc_spectr_name(plane_fft_wk)
!!      subroutine dealloc_horiz_spectr(plane_fft_wk)
!!        type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!!
!!      subroutine write_spectr_data(istep, plane_fft_wk)
!!      subroutine read_number_of_field(istep, plane_fft_wk)
!!      subroutine read_spectr_data(istep, plane_fft_wk)
!!      subroutine read_size_of_spectr(plane_fft_wk)
!!      subroutine write_size_of_spectr(merged, plane_fft_wk)
!
      module t_spectr_4_ispack
!
      use m_precision
!
      implicit    none
!
      type plane_spectr_by_ispack
        integer(kind = kint) :: num_fft
        integer(kind = kint) :: num_io
!
        integer(kind = kint) :: kx_max
        integer(kind = kint) :: ky_max
        integer(kind = kint) :: iz_max
        integer(kind = kint) :: num_spectr
!
        real   (kind=kreal), allocatable  ::  phys_d(:)
        real   (kind=kreal), allocatable  ::  phys_io(:)

        character(len=kchara), allocatable :: fft_name(:)
        character(len=kchara), allocatable :: fft_comp(:)
        integer(kind = kint), allocatable :: ifield_fft(:)
        integer(kind = kint), allocatable :: icomp_fft(:)

        real(kind=kreal), allocatable  ::  wk_pfft(:)
      end type plane_spectr_by_ispack
!
      integer(kind=kint ), parameter, private :: spectr_data_code = 21
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_spectr_name(plane_fft_wk)
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
       allocate( plane_fft_wk%fft_name(plane_fft_wk%num_fft) )
       allocate( plane_fft_wk%fft_comp(plane_fft_wk%num_fft) )
       allocate( plane_fft_wk%ifield_fft(plane_fft_wk%num_fft) )
       allocate( plane_fft_wk%icomp_fft(plane_fft_wk%num_fft) )
!
       plane_fft_wk%ifield_fft = 0
       plane_fft_wk%icomp_fft = 0
!
      end subroutine alloc_spectr_name
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_horiz_spectr(plane_fft_wk)
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
      integer(kind = kint) :: num
!
       plane_fft_wk%num_spectr = plane_fft_wk%kx_max                    &
      &                         * plane_fft_wk%ky_max                   &
      &                         * plane_fft_wk%iz_max
!
      num = plane_fft_wk%num_spectr * plane_fft_wk%num_fft
       allocate ( plane_fft_wk%phys_d(num))
       allocate ( plane_fft_wk%wk_pfft(num))

       plane_fft_wk%phys_d = 0.0d0
       plane_fft_wk%wk_pfft = 0.0d0
!
       end subroutine alloc_horiz_spectr
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_spectr_4_io(plane_fft_wk)
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
      integer(kind = kint) :: num
!
       plane_fft_wk%num_io = plane_fft_wk%kx_max                        &
     &                       * plane_fft_wk%ky_max                      &
     &                       * plane_fft_wk%iz_max
!
      num = plane_fft_wk%num_io * plane_fft_wk%num_fft
      allocate ( plane_fft_wk%phys_io(num) )
!
      plane_fft_wk%phys_io = 0.0d0
!
      end subroutine alloc_spectr_4_io
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_spectr_name(plane_fft_wk)
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
!
        deallocate( plane_fft_wk%fft_name )
        deallocate( plane_fft_wk%fft_comp )
        deallocate( plane_fft_wk%ifield_fft )
        deallocate( plane_fft_wk%icomp_fft )
!
!
       end subroutine dealloc_spectr_name
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_horiz_spectr(plane_fft_wk)
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
       deallocate ( plane_fft_wk%phys_d )
       deallocate ( plane_fft_wk%wk_pfft )
!
       end subroutine dealloc_horiz_spectr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_spectr_data(istep, plane_fft_wk)
!
      use m_constants
      use set_spectr_file_name
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      integer (kind = kint) :: istep
!
      integer (kind = kint) :: j
      integer (kind = kint) :: ix, iy, iz
      integer (kind = kint) :: ii1, ii2, ii3 , ii4
      character(len=kchara) ::  file_name
!
!
      call s_set_spectr_file_name(istep, file_name)
!
      write(*,*) 'merged spectral:     ', file_name
      open (spectr_data_code,  file=file_name, form='formatted',        &
     &                        status ='unknown')

      write(spectr_data_code,*) 'number_of_results',                    &
     &                         plane_fft_wk%num_fft
!
      do j = 1, plane_fft_wk%num_fft
!
       write(spectr_data_code,*) trim( plane_fft_wk%fft_name(j) )
       write(spectr_data_code,*) trim( plane_fft_wk%fft_comp(j) )
!
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                plane_fft_wk%phys_io(ii1), zero, zero, zero
        end do

       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &          + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2   = (j-1)*plane_fft_wk%num_spectr                          &
     &          + (2*ix+1)*plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &           plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),  &
     &           zero, zero
        end do
       end do
!
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &          + plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                plane_fft_wk%phys_io(ii1), zero, zero, zero
        end do

      do iy = 1, plane_fft_wk%kx_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) + iz
         ii3   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &            plane_fft_wk%phys_io(ii1), zero,                      &
     &            plane_fft_wk%phys_io(ii3), zero
        end do
       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) &
     &          + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) &
     &          + (2*ix+1)*plane_fft_wk%iz_max + iz
         ii3 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) &
     &          + (2*ix  )*plane_fft_wk%iz_max + iz
         ii4 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max) &
     &          + (2*ix+1)*plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &           plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),  &
     &           plane_fft_wk%phys_io(ii3), plane_fft_wk%phys_io(ii4)
        end do
       end do

        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + plane_fft_wk%iz_max + iz
         ii3 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &            plane_fft_wk%phys_io(ii1), zero,                      &
     &            plane_fft_wk%phys_io(ii3), zero
        end do
      end do

        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max) + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                  plane_fft_wk%phys_io(ii1), zero, zero, zero
        end do

       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &        + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &        + (2*ix+1)*plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &           plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),  &
     &           zero, zero
        end do
       end do

        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &          + plane_fft_wk%iz_max + iz
         write(spectr_data_code,'(1p4e20.11)')                          &
     &                  plane_fft_wk%phys_io(ii1), zero, zero, zero
        end do

      end do
!
      close(  spectr_data_code )
!
       end subroutine write_spectr_data

!
!  ---------------------------------------------------------------------
!
      subroutine read_number_of_field(istep, plane_fft_wk)
!
      use set_spectr_file_name
!
      integer (kind = kint), intent(in) :: istep
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      character (len=kchara) :: tmpchara
      character(len=kchara) ::  file_name
!
      call s_set_spectr_file_name(istep, file_name)
      write(*,*) 'spectral data:     ', file_name
      open (spectr_data_code,  file=file_name, form='formatted',        &
     &                        status ='unknown')

      read(spectr_data_code,*) tmpchara, plane_fft_wk%num_fft
!
      close(spectr_data_code)
!
!
       end subroutine read_number_of_field
!
!  ---------------------------------------------------------------------
!
      subroutine read_spectr_data(istep, plane_fft_wk)
!
      use set_spectr_file_name
!
      integer (kind = kint), intent(in) :: istep
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      integer (kind = kint) :: j
      integer (kind = kint) :: ix, iy, iz
      integer (kind = kint) :: ii1, ii2, ii3 , ii4
!
      real(kind = kreal)  :: rtmp
      character(len=kchara) :: tmpchara
      character(len=kchara) :: file_name
!
!
      call s_set_spectr_file_name(istep, file_name)
      write(*,*) 'spectral data:     ', file_name
      open (spectr_data_code,  file=file_name, form='formatted',        &
     &                        status ='old')

      plane_fft_wk%num_fft = 0
      read(spectr_data_code,*) tmpchara, plane_fft_wk%num_fft
!
      write(*,*) 'num_fft', plane_fft_wk%num_fft
      do j = 1, plane_fft_wk%num_fft
       plane_fft_wk%fft_name(j) = ''
       plane_fft_wk%fft_comp(j) = ''
      end do
!
      do j = 1, plane_fft_wk%num_fft
!
       read(spectr_data_code,*) plane_fft_wk%fft_name(j)
       read(spectr_data_code,*) plane_fft_wk%fft_comp(j)
!
       write(*,*) j, plane_fft_wk%fft_name(j), plane_fft_wk%fft_comp(j)
!
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr + iz
         read(spectr_data_code,*)                                       &
     &         plane_fft_wk%phys_io(ii1), rtmp, rtmp, rtmp
        end do
!
       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (2*ix+1)*plane_fft_wk%iz_max + iz
         read(spectr_data_code,*)                                       &
     &        plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),     &
     &        rtmp, rtmp
        end do
       end do
!
        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr + plane_fft_wk%iz_max + iz
         read(spectr_data_code,*) plane_fft_wk%phys_io(ii1),            &
     &                           rtmp, rtmp, rtmp
        end do

      do iy = 1, plane_fft_wk%kx_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + iz
         ii3 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + iz
         read(spectr_data_code,*)                                       &
     &            plane_fft_wk%phys_io(ii1), rtmp,                      &
     &            plane_fft_wk%phys_io(ii3), rtmp
        end do
       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + (2*ix+1)*plane_fft_wk%iz_max + iz
         ii3 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + (2*ix  )*plane_fft_wk%iz_max + iz
         ii4 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + (2*ix+1)*plane_fft_wk%iz_max + iz
         read(spectr_data_code,*)                                       &
     &          plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),   &
     &          plane_fft_wk%phys_io(ii3), plane_fft_wk%phys_io(ii4)
        end do
       end do

        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy  )*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + plane_fft_wk%iz_max + iz
         ii3 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (2*iy+1)*(plane_fft_wk%iz_max*plane_fft_wk%kx_max)      &
     &          + plane_fft_wk%iz_max + iz
         read(spectr_data_code,*)                                       &
     &            plane_fft_wk%phys_io(ii1), rtmp,                      &
     &            plane_fft_wk%phys_io(ii3), rtmp
        end do
      end do

        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max) + iz
         read(spectr_data_code,*)  plane_fft_wk%phys_io(ii1),           &
     &                             rtmp, rtmp, rtmp
        end do

       do ix = 1, plane_fft_wk%ky_max/2-1
        do iz = 1, plane_fft_wk%iz_max
         ii1 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &        + (2*ix  )*plane_fft_wk%iz_max + iz
         ii2 = (j-1)*plane_fft_wk%num_spectr                            &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &        + (2*ix+1)*plane_fft_wk%iz_max + iz
         read(spectr_data_code,*)                                       &
     &           plane_fft_wk%phys_io(ii1), plane_fft_wk%phys_io(ii2),  &
     &           rtmp, rtmp
        end do
       end do

        do iz = 1, plane_fft_wk%iz_max
         ii1   = (j-1)*plane_fft_wk%num_spectr                          &
     &        + (plane_fft_wk%iz_max*plane_fft_wk%kx_max)               &
     &          + plane_fft_wk%iz_max + iz
         read(spectr_data_code,*)                                       &
     &                  plane_fft_wk%phys_io(ii1), rtmp, rtmp, rtmp
        end do

      end do
!
      close(  spectr_data_code )
!
       end subroutine read_spectr_data
!
! -----------------------------------------------------------------------
!
      subroutine read_size_of_spectr(plane_fft_wk)
!
      use set_spectr_file_name
!
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
       real(kind = kreal) :: ztmp
       integer (kind = kint) :: ix, iy, iz
!
!    read outlines of spectr data
!
      open (spectr_data_code,  file=spec_mode_file_name,                &
     &                       form='formatted',  status ='unknown')
       
!
      plane_fft_wk%kx_max = 0
      plane_fft_wk%ky_max = 0
      plane_fft_wk%iz_max = 0
      read(spectr_data_code,*) 
      do
       read(spectr_data_code,*, err = 99, end = 99) ix, iy, iz, ztmp
       plane_fft_wk%kx_max = max(plane_fft_wk%kx_max,ix)
       plane_fft_wk%ky_max = max(plane_fft_wk%ky_max,iy)
       plane_fft_wk%iz_max = max(plane_fft_wk%iz_max,iz)
      end do
  99  continue
!
      close(spectr_data_code)
!
      write(*,*) 'truncation level(x,y) and grid points (z)'
      write(*,*) plane_fft_wk%kx_max, plane_fft_wk%ky_max,              &
     &          plane_fft_wk%iz_max
!
      plane_fft_wk%kx_max = plane_fft_wk%kx_max*2
      plane_fft_wk%ky_max = plane_fft_wk%ky_max*2
      plane_fft_wk%num_spectr = plane_fft_wk%kx_max                     &
    &                    * plane_fft_wk%ky_max * plane_fft_wk%iz_max
!
      return
      end subroutine read_size_of_spectr
!
!-----------------------------------------------------------------------
!
      subroutine write_size_of_spectr(merged, plane_fft_wk)
!
      use t_mesh_data
      use set_spectr_file_name
!
      type(mesh_geometry), intent(in) :: merged
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      integer(kind=kint ) :: ix,iy,iz,iii
!
      open (spectr_data_code,  file=spec_mode_file_name,                &
     &                       form='formatted', status ='unknown')
       
!
      write(spectr_data_code,*) ' kx, ky, z_id, z'
      do iy = 0, plane_fft_wk%kx_max/2
        do ix = 0, plane_fft_wk%ky_max/2
          do iz = 1, plane_fft_wk%iz_max
            iii = iz*plane_fft_wk%kx_max*plane_fft_wk%ky_max
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
      end module t_spectr_4_ispack
