!gz_multi_ucd_file_IO.f90
!      module gz_multi_ucd_file_IO
!
!      Written by H. Matsui on July, 2006
!
!      subroutine output_gz_multi_udt_file(file_name,                   &
!     &         ntot_out, ist_nod, ied_nod, inod_out,                   &
!     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!      subroutine output_gz_multi_grd_file(file_name,                   &
!     &         ntot_out, ist_nod, ied_nod, inod_out, xx_out,           &
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         ncomp_dat)
!      subroutine output_gz_multi_ucd_file(file_name,                   &
!     &         ntot_out, ist_nod, ied_nod, inod_out, xx_out,           &
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      module gz_multi_ucd_file_IO
!
      use m_precision
      use m_constants
      use set_parallel_file_name
      use skip_gz_comment
!
      implicit  none
!
      character(len=kchara), private :: gzip_name
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_gz_multi_udt_file(file_name,                    &
     &         ntot_out, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      use gz_ucd_data_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint), intent(in) :: ntot_out, ncomp_dat
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: dat_out(ntot_out, ncomp_dat)
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      write(*,*) 'write gzipped udt data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_udt_field_header(num_output, ncomp_out, name_out)
      call write_gz_multi_udt_data(ntot_out, ist_nod, ied_nod,          &
     &   ncomp_dat, inod_out, dat_out)
!
      call close_gzfile
!
      end subroutine output_gz_multi_udt_file
!
! ----------------------------------------------------------------------
!
      subroutine output_gz_multi_grd_file(file_name,                    &
     &         ntot_out, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         ncomp_dat)
!
      use gz_ucd_data_IO
!
      character(len = kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: ntot_out, ncomp_dat
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: xx_out(ntot_out, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint), intent(in) :: ie_out(ntot_ele,nnod_4_ele)
!
      integer(kind = kint) :: nnod, nele
!
!
      nnod = ied_nod - ist_nod + 1
      nele = ied_ele - ist_ele + 1
      call add_gzip_extension(file_name, gzip_name)
!
      write(*,*) 'write gzipped grd data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_udt_mesh_header(nnod, nele, ncomp_dat)
      call write_gz_multi_udt_data(ntot_out, ist_nod, ied_nod,          &
     &    ithree, inod_out, xx_out)
      call write_gz_multi_grd_connect(nnod_4_ele, ntot_ele,             &
     &    ist_ele, ied_ele, iele_gl, ie_out)
!
      call close_gzfile
!
      end subroutine output_gz_multi_grd_file
!
! ----------------------------------------------------------------------
!
      subroutine output_gz_multi_ucd_file(file_name,                    &
     &         ntot_out, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      use gz_ucd_data_IO
!
      character(len = kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: ntot_out
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: xx_out(ntot_out, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint), intent(in) :: ie_out(ntot_ele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: num_output, ncomp_dat
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
      real(kind = kreal), intent(in) :: dat_out(ntot_out, ncomp_dat)
!
      integer(kind = kint) :: nnod, nele
!
!
      nnod = ied_nod - ist_nod + 1
      nele = ied_ele - ist_ele + 1
      call add_gzip_extension(file_name, gzip_name)
!
      write(*,*) 'write gzipped UCD data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_udt_mesh_header(nnod, nele, ncomp_dat)
      call write_gz_multi_udt_data(ntot_out, ist_nod, ied_nod,          &
     &    ithree, inod_out, xx_out)
      call write_gz_multi_grd_connect(nnod_4_ele, ntot_ele,             &
     &    ist_ele, ied_ele, iele_gl, ie_out)
!
      call write_gz_udt_field_header(num_output, ncomp_out, name_out)
      call write_gz_multi_udt_data(ntot_out, ist_nod, ied_nod,          &
     &    ncomp_dat, inod_out, dat_out)
!
      call close_gzfile
!
      end subroutine output_gz_multi_ucd_file
!
! ----------------------------------------------------------------------
!
      end module gz_multi_ucd_file_IO

