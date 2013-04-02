!read_psf_result.f90
!      module read_psf_result
!
!      Written by H. Matsui
!
!      subroutine read_allocate_psf_ucd(psf_udt_name)
!      subroutine read_allocate_psf_udt(psf_udt_name)
!
!      subroutine read_psf_grd(psf_grid_name)
!      subroutine read_psf_data_udt(psf_udt_name)
!
!      subroutine read_allocate_psf_ncomps_udt
!
!      subroutine read_psf_field_name
!
      module read_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
!
      implicit none
!
      private :: read_psf_header, read_psf_grid, read_psf_data
      private :: read_and_count_psf_ncomp, read_psf_num_comp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_ucd(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'PSF UCD data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted',          &
     &     status='old')
!
      call read_psf_header
      call allocate_psf_results
      call read_psf_grid
!
      call read_allocate_psf_ncomps_udt
      call read_psf_data
!
      close(id_psf_result)
!
      end subroutine read_allocate_psf_ucd
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_grd(psf_grid_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: psf_grid_name
!
!
      write(*,*) 'PSF grid data: ', trim(psf_grid_name)
      open(id_psf_result, file=psf_grid_name, form='formatted',         &
     &     status='old')
!
      call read_psf_header
      call allocate_psf_results
!
      call read_psf_grid
!
      close(id_psf_result)
!
      end subroutine read_psf_grd
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_udt(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'PSF result data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted',          &
     &     status='old')
!
      call read_allocate_psf_ncomps_udt
      call read_psf_data
!
      close(id_psf_result)
!
      end subroutine read_allocate_psf_udt
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_data_udt(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
!
!
      write(*,*) 'PSF result data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted',          &
     &     status='old')
!
      call read_psf_num_comp
      call count_stack_tot_psf_field
      call read_psf_data
!
      close(id_psf_result)
!
      end subroutine read_psf_data_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_ncomps_udt
!
!
      call read_and_count_psf_ncomp
      call count_stack_tot_psf_field
      call allocate_psf_field_data
!
      end subroutine read_allocate_psf_ncomps_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_psf_header
!
      integer(kind = kint) :: itmp
!
      read(id_psf_result,*) numnod_psf, numele_psf, ncomptot_psf,       &
     &                     itmp, itmp
!
      end subroutine read_psf_header
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_psf_grid
!
      integer(kind = kint) :: i
      integer(kind = kint) :: itmp
      character(len=kchara) :: tmpchara
!
!
!
      do i = 1, numnod_psf
        read(id_psf_result,*) inod_psf(i), xx_psf(i,1:3)
      end do
!
!      write(*,*) 'finish node data'
!
      do i = 1, numele_psf
        read(id_psf_result,*) iele_psf(i), itmp,                        &
     &                        tmpchara, ie_psf(i,1:3)
      end do
!
      end subroutine read_psf_grid
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_field_name
!
      integer(kind = kint) :: i
!
!
      do i = 1, nfield_psf
        read(id_psf_result,*) psf_data_name(i)
      end do
!
!      write(*,*) 'finish field header', nfield_psf,                    &
!     &        psf_data_name(1:nfield_psf)
!
      end subroutine read_psf_field_name
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_data
!
      integer(kind = kint) :: i
      integer(kind = kint) :: itmp
!
!
      call read_psf_field_name
!
      do i = 1, numnod_psf
        read(id_psf_result,*) itmp, d_nod_psf(i,1:ncomptot_psf)
      end do
!
      end subroutine read_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_and_count_psf_ncomp
!
      character(len=255) :: tmpchara
      integer(kind = kint) :: i, itmp, itot
!
!
      read(id_psf_result,'(a)') tmpchara
      read(tmpchara,*) nfield_psf
!
      call allocate_psf_num_field
!
      read(tmpchara,*,err=10) nfield_psf, ncomp_psf(1:nfield_psf)
      return
!
!
  10  continue
      do i = 1, nfield_psf
        if( ncomp_psf(i) .lt. 0) then
          itmp = i
          exit
        end if
      end do
!
      read(id_psf_result,*) ncomp_psf(itmp:nfield_psf)
      return
!
      end subroutine read_and_count_psf_ncomp
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_num_comp
!
      integer(kind = kint) :: i
!
!
      ncomp_psf = -1
      read(id_psf_result,*) nfield_psf, ncomp_psf(1:nfield_psf)
!
      end subroutine read_psf_num_comp
!
!-----------------------------------------------------------------------
!
      end module  read_psf_result
