!gz_read_psf_result.f90
!      module gz_read_psf_result
!
!      Written by H. Matsui
!
!      subroutine read_allocate_psf_ucd_gz(gzip_name)
!      subroutine read_allocate_psf_udt_gz(gzip_name)
!
!      subroutine read_alloc_psf_grd_gz(gzip_name)
!
      module gz_read_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
      use gz_ucd_data_IO
!
      implicit none
!
      private :: read_alloc_psf_grd_data_gz, read_alloc_psf_udt_data_gz
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_ucd_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'read gzipped ucd file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_alloc_psf_grd_data_gz
      call read_alloc_psf_udt_data_gz
!
      call close_gzfile
!
      end subroutine read_allocate_psf_ucd_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_psf_grd_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'Read gzipped ucd  grid file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_alloc_psf_grd_data_gz
      call close_gzfile
!
      end subroutine read_alloc_psf_grd_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_udt_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'Read gzipped ucd data file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_alloc_psf_udt_data_gz
      call close_gzfile
!
      end subroutine read_allocate_psf_udt_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_alloc_psf_grd_data_gz
!
!
      call read_gz_udt_mesh_header(numnod_psf, numele_psf,              &
     &    ncomptot_psf)
!
      call allocate_psf_results
!
      call read_gz_udt_field_data(numnod_psf, ithree, xx_psf)
      call read_gz_ucd_mesh_connect(numele_psf, ithree,                 &
     &          iele_psf, ie_psf)
!
      end subroutine read_alloc_psf_grd_data_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_psf_udt_data_gz
!
!
      call read_gz_udt_field_num(nfield_psf)
      call allocate_psf_num_field
!
      call read_gz_udt_field_name(nfield_psf, ncomp_psf, psf_data_name)
      call count_stack_tot_psf_field
      call allocate_psf_field_data
!
      call read_gz_udt_field_name(nfield_psf, ncomp_psf, psf_data_name)
      call read_gz_udt_field_data(numnod_psf, ncomptot_psf, d_nod_psf)
!
      end subroutine read_alloc_psf_udt_data_gz
!
!-----------------------------------------------------------------------
!
      end module  gz_read_psf_result
