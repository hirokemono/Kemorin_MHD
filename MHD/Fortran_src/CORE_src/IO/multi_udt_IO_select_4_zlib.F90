!multi_udt_IO_select_4_zlib.F90
!      module multi_udt_IO_select_4_zlib
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_write_multi_udt_file(file_header, itype_ucd_file, &
!     &          istep_ucd, ntot_nod, ist_nod, ied_nod, inod_out,       &
!     &          num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!      subroutine sel_write_multi_grd_file(file_header, itype_ucd_file, &
!     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,           &
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         ncomp_dat)
!      subroutine sel_write_multi_ucd_file(file_header, itype_ucd_file, &
!     &         istep_ucd, ntot_nod, ist_nod, ied_nod, inod_out, xx_out,&
!     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out,&
!     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
!
      module multi_udt_IO_select_4_zlib
!
      use m_precision
      use m_constants
      use m_field_file_format
!
      use set_parallel_file_name
      use set_ucd_file_names
      use multi_ucd_file_IO
      use gz_multi_ucd_file_IO
      use vtk_file_IO
      use gz_vtk_file_IO
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_ucd_file = 16
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_write_multi_udt_file(file_header, itype_ucd_file,  &
     &          istep_ucd, ntot_nod, ist_nod, ied_nod, inod_out,        &
     &          num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      character(len = kchara), intent(in) :: file_header
      integer(kind=kint), intent(in) :: itype_ucd_file, istep_ucd
!
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint), intent(in) :: ntot_nod, ncomp_dat
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      integer(kind = kint), intent(in) :: inod_out(ntot_nod)
      real(kind = kreal), intent(in) :: dat_out(ntot_nod, ncomp_dat)
!
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(file_header, itype_ucd_file,        &
          istep_ucd, file_name)
!
      if(itype_ucd_file .eq. iflag_udt) then
        call output_multi_udt_file(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtd) then
        call output_multi_vtk_phys(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
#ifdef ZLIB_IO
      else if(itype_ucd_file .eq. iflag_udt_gz) then
        call output_gz_multi_udt_file(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtd_gz) then
        call output_gz_multi_vtk_phys(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
#endif
      end if
!
      end subroutine sel_write_multi_udt_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_multi_grd_file(file_header, itype_ucd_file,  &
     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         ncomp_dat)
!
      character(len = kchara), intent(in) :: file_header
      integer(kind = kint), intent(in) :: itype_ucd_file
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
      character(len=kchara) :: file_name
!
!
      call set_single_grd_file_name(file_header, itype_ucd_file,        &
          file_name)
!
      if(itype_ucd_file .eq. iflag_udt) then
        call output_multi_grd_file(file_name, id_ucd_file, ntot_nod,    &
     &      ist_nod, ied_nod, inod_out, xx_out, ntot_ele, nnod_4_ele,   &
     &      ist_ele, ied_ele, iele_gl, ie_out, ncomp_dat)
      else if(itype_ucd_file .eq. iflag_vtd) then
        call output_multi_vtk_grid(file_name, id_ucd_file, ntot_nod,    &
     &      ist_nod, ied_nod, xx_out, ntot_ele, nnod_4_ele,             &
     &      ist_ele, ied_ele, ie_out)
!
#ifdef ZLIB_IO
      else if(itype_ucd_file .eq. iflag_udt_gz) then
        call output_gz_multi_grd_file(file_name, ntot_nod,              &
     &      ist_nod, ied_nod, inod_out, xx_out, ntot_ele, nnod_4_ele,   &
     &      ist_ele, ied_ele, iele_gl, ie_out, ncomp_dat)
      else if(itype_ucd_file .eq. iflag_vtd_gz) then
        call output_gz_multi_vtk_grid(file_name, ntot_nod,              &
     &      ist_nod, ied_nod, xx_out, ntot_ele, nnod_4_ele,             &
     &      ist_ele, ied_ele, ie_out)
#endif
      end if
!
      end subroutine sel_write_multi_grd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_multi_ucd_file(file_header, itype_ucd_file,  &
     &         istep_ucd, ntot_nod, ist_nod, ied_nod, inod_out, xx_out, &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
      character(len = kchara), intent(in) :: file_header
      integer(kind=kint), intent(in) :: itype_ucd_file, istep_ucd
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
      character(len=kchara) :: file_name
!
!
      call set_single_ucd_file_name(file_header, itype_ucd_file,        &
          istep_ucd, file_name)
!
      if     (itype_ucd_file .eq. iflag_ucd) then
        call output_multi_ucd_file(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_udt) then
        call output_multi_udt_file(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtk) then
        call output_multi_vtk_file(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod, xx_out,                      &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, ie_out,          &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtd) then
        call output_multi_vtk_phys(file_name, id_ucd_file,              &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
!
#ifdef ZLIB_IO
      else if(itype_ucd_file .eq. iflag_ucd_gz) then
        call output_gz_multi_ucd_file(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod, inod_out, xx_out,            &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, iele_gl, ie_out, &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_udt_gz) then
        call output_gz_multi_udt_file(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod, inod_out,                    &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtk_gz) then
        call output_multi_gz_vtk_file(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod, xx_out,                      &
     &         ntot_ele, nnod_4_ele, ist_ele, ied_ele, ie_out,          &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
      else if(itype_ucd_file .eq. iflag_vtd_gz) then
        call output_gz_multi_vtk_phys(file_name,                        &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_output, ncomp_dat, ncomp_out, name_out, dat_out)
#endif
      end if
!
      end subroutine sel_write_multi_ucd_file
!
!------------------------------------------------------------------
!
      end module multi_udt_IO_select_4_zlib
