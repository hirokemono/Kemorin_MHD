!>@file   read_psf_result.f90
!!        module read_psf_result
!!
!! @author H. Matsui
!! @date   Programmed in ????
!!
!
!> @brief read psf results
!!
!!@verbatim
!!      subroutine read_allocate_psf_ucd(psf_udt_name)
!!      subroutine read_allocate_psf_udt(psf_udt_name)
!!
!!      subroutine read_psf_grd(psf_grid_name)
!!@endverbatim
!!
!!@n @param psf_udt_name    file name of cross section field data
!!@n @param psf_grid_name   file name of cross section mesh data
!
      module read_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
      use udt_data_IO
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_psf_result = 7
!
      private :: read_alloc_psf_grd_data, read_alloc_psf_udt_data
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
!
!
      write(*,*) 'PSF UCD data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted',          &
     &     status='old')
!
      call read_alloc_psf_grd_data
      call read_alloc_psf_udt_data
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
      call read_alloc_psf_grd_data
      close(id_psf_result)
!
      end subroutine read_psf_grd
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_udt(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
!
!
      write(*,*) 'PSF result data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted',          &
     &     status='old')
!
      call read_alloc_psf_udt_data
!
      close(id_psf_result)
!
      end subroutine read_allocate_psf_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_alloc_psf_grd_data
!
!
      call read_ucd_mesh_header(id_psf_result, numnod_psf, numele_psf,  &
     &    ncomptot_psf)
!
      call allocate_psf_results
      call read_ucd_mesh_data(id_psf_result, numnod_psf, numele_psf,    &
     &          ithree, inod_psf, iele_psf, xx_psf, ie_psf)
!
      end subroutine read_alloc_psf_grd_data
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_psf_udt_data
!
!
      read(id_psf_result,*) nfield_psf
      backspace(id_psf_result)
!
      call allocate_psf_num_field
      call read_udt_field_header(id_psf_result, nfield_psf,             &
     &    ncomp_psf, psf_data_name)
!
      call count_stack_tot_psf_field
      call allocate_psf_field_data
!
      call read_ucd_field_data(id_psf_result, numnod_psf, ncomptot_psf, &
     &    inod_psf, d_nod_psf)
!
      end subroutine read_alloc_psf_udt_data
!
!-----------------------------------------------------------------------
!
      end module  read_psf_result
