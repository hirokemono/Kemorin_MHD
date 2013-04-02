!write_psf_result.f90
!      module write_psf_result
!
!      Written by H. Matsui
!
!      subroutine write_psf_ucd(psf_udt_name)
!      subroutine write_psf_grd(psf_grid_name)
!      subroutine write_psf_udt(psf_udt_name)
!
      module write_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
!
      implicit none
!
      private :: write_psf_grid, write_psf_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_ucd(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
!
!
      write(*,*) 'PSF UCD data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted')
!
      call write_psf_grid
      call write_psf_data
!
      close(id_psf_result)
!
      end subroutine write_psf_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_psf_grd(psf_grid_name)
!
      character(len=kchara), intent(in) :: psf_grid_name
!
!
      write(*,*) 'PSF grid data: ', trim(psf_grid_name)
      open(id_psf_result, file=psf_grid_name, form='formatted')
!
      call write_psf_grid
!
      close(id_psf_result)
!
      end subroutine write_psf_grd
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_udt(psf_udt_name)
!
      character(len=kchara), intent(in) :: psf_udt_name
!
!
      write(*,*) 'PSF result data: ', trim(psf_udt_name)
      open(id_psf_result, file=psf_udt_name, form='formatted')
!
      call write_psf_data
!
      close(id_psf_result)
!
      end subroutine write_psf_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_psf_grid
!
      use m_geometry_constants
      use udt_data_IO
!
!
      call write_udt_mesh_header(id_psf_result, numnod_psf,             &
     &    numele_psf, ncomptot_psf)
!
      call write_single_udt_data(id_psf_result, numnod_psf,             &
     &    ithree, inod_psf, xx_psf)
      call write_single_grd_connect(id_psf_result, num_triangle,        &
     &    numele_psf, iele_psf, ie_psf)
!
      end subroutine write_psf_grid
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_data
!
      use udt_data_IO
!
!
      call write_udt_field_header(id_psf_result, nfield_psf,            &
     &    ncomp_psf, psf_data_name)
      call write_single_udt_data(id_psf_result, numnod_psf,             &
     &    ncomptot_psf, inod_psf, d_nod_psf)
!
      end subroutine write_psf_data
!
!-----------------------------------------------------------------------
!
      end module  write_psf_result
