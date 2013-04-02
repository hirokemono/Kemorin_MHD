!multi_ucd_file_IO.f90
!      module multi_ucd_file_IO
!
!      Written by H. Matsui on July, 2006
!
!      subroutine output_multi_udt_file(file_name, id_ucd,              &
!     &         ntot_nod, ist_nod, ied_nod, inod_out,                   &
!     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!      subroutine output_multi_grd_file(file_name, id_ucd,              &
!     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,           &
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         ncomp_dat)
!      subroutine output_multi_ucd_file(file_name, id_ucd,              &
!     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,           &
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      module multi_ucd_file_IO
!
      use m_precision
      use m_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_udt_file(file_name, id_ucd,               &
     &         ntot_nod, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      use udt_data_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_ucd
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint), intent(in) :: ntot_nod, ncomp_dat
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_nod)
      real(kind = kreal), intent(in) :: dat_out(ntot_nod, ncomp_dat)
!
!
      write(*,*) 'write ascii UCD field data: ', trim(file_name)
      open (id_ucd, file=file_name, form='formatted')
!
      call write_udt_field_header(id_ucd, num_output,                   &
     &    ncomp_out, name_out)
      call write_multi_udt_data(id_ucd, ntot_nod, ist_nod, ied_nod,     &
     &          ncomp_dat, inod_out, dat_out)
!
      close(id_ucd)
!
      end subroutine output_multi_udt_file
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_grd_file(file_name, id_ucd,               &
     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         ncomp_dat)
!
      use udt_data_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_ucd
!
      integer(kind = kint), intent(in) :: ntot_nod, ncomp_dat
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_nod)
      real(kind = kreal), intent(in) :: xx_out(ntot_nod, 3)
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
      write(*,*) 'write ascii UCD grid data: ', trim(file_name)
      open (id_ucd, file=file_name, form='formatted')
!
      call write_udt_mesh_header(id_ucd, nnod, nele, ncomp_dat)
      call write_multi_udt_data(id_ucd, ntot_nod, ist_nod, ied_nod,     &
     &    ithree, inod_out, xx_out)
      call write_multi_grd_connect(id_ucd, nnod_4_ele, ntot_ele,        &
     &    ist_ele, ied_ele, iele_gl, ie_out)
!
      close(id_ucd)
!
      end subroutine output_multi_grd_file
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_ucd_file(file_name, id_ucd,               &
     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      use udt_data_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_ucd
!
      integer(kind = kint), intent(in) :: ntot_nod
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_nod)
      real(kind = kreal), intent(in) :: xx_out(ntot_nod, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint), intent(in) :: ie_out(ntot_ele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: num_output, ncomp_dat
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
      real(kind = kreal), intent(in) :: dat_out(ntot_nod, ncomp_dat)
!
      integer(kind = kint) :: nnod, nele
!
!
      nnod = ied_nod - ist_nod + 1
      nele = ied_ele - ist_ele + 1
!
      write(*,*) 'write ascii UCD data: ', trim(file_name)
      open (id_ucd, file=file_name, form='formatted')
!
      call write_udt_mesh_header(id_ucd, nnod, nele, ncomp_dat)
      call write_multi_udt_data(id_ucd, ntot_nod, ist_nod, ied_nod,     &
     &    ithree, inod_out, xx_out)
      call write_multi_grd_connect(id_ucd, nnod_4_ele, ntot_ele,        &
     &    ist_ele, ied_ele, iele_gl, ie_out)
!
      call write_udt_field_header(id_ucd, num_output, ncomp_out,        &
     &    name_out)
      call write_multi_udt_data(id_ucd, ntot_nod, ist_nod, ied_nod,     &
     &    ncomp_dat, inod_out, dat_out)
!
      close(id_ucd)
!
      end subroutine output_multi_ucd_file
!
! ----------------------------------------------------------------------
!
      end module multi_ucd_file_IO
