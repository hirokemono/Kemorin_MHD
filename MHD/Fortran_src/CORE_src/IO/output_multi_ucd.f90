!output_multi_ucd.f90
!      module output_multi_ucd
!
!      Written by H. Matsui on July, 2006
!
!      subroutine output_multi_ucd_grids
!      subroutine output_multi_ucd_fields(istep_ucd)
!
      module output_multi_ucd
!
      use m_precision
!
      implicit  none
!
      private ::  output_multi_grds, output_multi_ucds
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_ucd_grids
!
      use m_multi_ucd_data
!
!
      call output_multi_grds                                            &
     &       (num_mul_ucd, mul_ucd_header, itype_mul_ucd_file,          &
     &        num_comp_mul_ucd, istack_nod_mul_ucd, istack_ele_mul_ucd, &
     &        ntot_nod_mul_ucd, inod_mul_ucd, xx_mul_ucd,               &
     &        ntot_ele_mul_ucd, nnod_4_ele_mul_ucd, iele_mul_ucd,       &
     &        ie_mul_ucd)
!
      end subroutine output_multi_ucd_grids
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_ucd_fields(istep_ucd)
!
      use m_multi_ucd_data
!
      integer(kind = kint), intent(in) :: istep_ucd
!
!
      call output_multi_ucds(istep_ucd, num_mul_ucd,                    &
     &          mul_ucd_header, itype_mul_ucd_file,                     &
     &          num_fld_mul_ucd, num_comp_mul_ucd, istack_fld_mul_ucd,  &
     &          ntot_fld_mul_ucd_out, ncomp_mul_ucd, fld_name_mul_ucd,  &
     &          istack_nod_mul_ucd, istack_ele_mul_ucd,                 &
     &          ntot_nod_mul_ucd, inod_mul_ucd, xx_mul_ucd,             &
     &          ntot_ele_mul_ucd, nnod_4_ele_mul_ucd, iele_mul_ucd,     &
     &          ie_mul_ucd, max_comp_mul_ucd, dat_mul_ucd)
!
      end subroutine output_multi_ucd_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_multi_grds                                      &
     &       (num_ucd, mul_ucd_header, itype_mul_ucd_file,              &
     &        num_comp_mul_ucd, istack_nod_mul_ucd, istack_ele_mul_ucd, &
     &        ntot_nod_mul_ucd, inod_mul_ucd, xx_mul_ucd,               &
     &        ntot_ele_mul_ucd, nnod_4_ele_mul_ucd, iele_mul_ucd,       &
     &        ie_mul_ucd)
!
      use multi_udt_IO_select_4_zlib
!
      integer(kind = kint), intent(in) :: num_ucd
      character(len = kchara), intent(in) :: mul_ucd_header(num_ucd)
      integer(kind=kint), intent(in) :: itype_mul_ucd_file(num_ucd)
!
      integer(kind = kint), intent(in) :: num_comp_mul_ucd(num_ucd)
!
      integer(kind = kint), intent(in) :: istack_nod_mul_ucd(0:num_ucd)
      integer(kind = kint), intent(in) :: istack_ele_mul_ucd(0:num_ucd)
!
      integer(kind = kint), intent(in) :: ntot_nod_mul_ucd
      integer(kind = kint), intent(in)                                  &
     &              :: inod_mul_ucd(ntot_nod_mul_ucd)
      real(kind = kreal), intent(in) :: xx_mul_ucd(ntot_nod_mul_ucd,3)
!
      integer(kind = kint), intent(in) :: ntot_ele_mul_ucd
      integer(kind = kint), intent(in) :: nnod_4_ele_mul_ucd
      integer(kind = kint), intent(in)                                  &
     &            :: iele_mul_ucd(ntot_ele_mul_ucd)
      integer(kind = kint), intent(in)                                  &
     &            :: ie_mul_ucd(ntot_ele_mul_ucd,nnod_4_ele_mul_ucd)
!
      integer(kind = kint) :: i_ucd
      integer(kind = kint) :: ist_nod, ied_nod
      integer(kind = kint) :: ist_ele, ied_ele
!
!
      do i_ucd = 1, num_ucd
        ist_nod = istack_nod_mul_ucd(i_ucd-1) + 1
        ied_nod = istack_nod_mul_ucd(i_ucd  )
        ist_ele = istack_ele_mul_ucd(i_ucd-1) + 1
        ied_ele = istack_ele_mul_ucd(i_ucd  )
!
        call sel_write_multi_grd_file(mul_ucd_header(i_ucd),            &
     &     itype_mul_ucd_file(i_ucd), ntot_nod_mul_ucd,                 &
     &     ist_nod, ied_nod, inod_mul_ucd, xx_mul_ucd,                  &
     &     ntot_ele_mul_ucd, nnod_4_ele_mul_ucd, ist_ele, ied_ele,      &
     &     iele_mul_ucd, ie_mul_ucd, num_comp_mul_ucd(i_ucd) )
      end do
!
      end subroutine output_multi_grds
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_ucds                                      &
     &         (istep_ucd, num_ucd, mul_ucd_header, itype_mul_ucd_file, &
     &          num_fld_mul_ucd, num_comp_mul_ucd, istack_fld_mul_ucd,  &
     &          ntot_fld_mul_ucd_out, ncomp_mul_ucd, fld_name_mul_ucd,  &
     &          istack_nod_mul_ucd, istack_ele_mul_ucd,                 &
     &          ntot_nod_mul_ucd, inod_mul_ucd, xx_mul_ucd,             &
     &          ntot_ele_mul_ucd, nnod_4_ele_mul_ucd, iele_mul_ucd,     &
     &          ie_mul_ucd, max_comp_mul_ucd, dat_mul_ucd)
!
      use multi_udt_IO_select_4_zlib
!
      integer(kind = kint), intent(in) :: istep_ucd
!
      integer(kind = kint), intent(in) :: num_ucd
      character(len = kchara), intent(in) :: mul_ucd_header(num_ucd)
      integer(kind=kint), intent(in) :: itype_mul_ucd_file(num_ucd)
!
      integer(kind = kint), intent(in) :: num_fld_mul_ucd(num_ucd)
      integer(kind = kint), intent(in) :: num_comp_mul_ucd(num_ucd)
      integer(kind = kint), intent(in) :: istack_fld_mul_ucd(0:num_ucd)

      integer(kind = kint), intent(in) :: ntot_fld_mul_ucd_out
      integer(kind = kint), intent(in)                                  &
     &            :: ncomp_mul_ucd(ntot_fld_mul_ucd_out)
      character(len = kchara), intent(in)                               &
     &            :: fld_name_mul_ucd(ntot_fld_mul_ucd_out)
!
      integer(kind = kint), intent(in) :: istack_nod_mul_ucd(0:num_ucd)
      integer(kind = kint), intent(in) :: istack_ele_mul_ucd(0:num_ucd)
!
      integer(kind = kint), intent(in) :: ntot_nod_mul_ucd
      integer(kind = kint), intent(in)                                  &
     &              :: inod_mul_ucd(ntot_nod_mul_ucd)
      real(kind = kreal), intent(in) :: xx_mul_ucd(ntot_nod_mul_ucd,3)
!
      integer(kind = kint), intent(in) :: ntot_ele_mul_ucd
      integer(kind = kint), intent(in) :: nnod_4_ele_mul_ucd
      integer(kind = kint), intent(in)                                  &
     &            :: iele_mul_ucd(ntot_ele_mul_ucd)
      integer(kind = kint), intent(in)                                  &
     &            :: ie_mul_ucd(ntot_ele_mul_ucd,nnod_4_ele_mul_ucd)
!
      integer(kind = kint), intent(in) :: max_comp_mul_ucd
      real(kind = kreal), intent(in)                                    &
     &            :: dat_mul_ucd(ntot_nod_mul_ucd, max_comp_mul_ucd)
!
      integer(kind = kint) :: i_ucd,   ist_ucd
      integer(kind = kint) :: ist_nod, ied_nod
      integer(kind = kint) :: ist_ele, ied_ele
!
!
      do i_ucd = 1, num_ucd
        ist_ucd = istack_fld_mul_ucd(i_ucd-1) + 1
        ist_nod = istack_nod_mul_ucd(i_ucd-1) + 1
        ied_nod = istack_nod_mul_ucd(i_ucd  )
        ist_ele = istack_ele_mul_ucd(i_ucd-1) + 1
        ied_ele = istack_ele_mul_ucd(i_ucd  )
!
        call sel_write_multi_ucd_file(mul_ucd_header(i_ucd),            &
     &      itype_mul_ucd_file(i_ucd), istep_ucd, ntot_nod_mul_ucd,     &
     &      ist_nod, ied_nod, inod_mul_ucd, xx_mul_ucd,                 &
     &      ntot_ele_mul_ucd, nnod_4_ele_mul_ucd,                       &
     &      ist_ele, ied_ele, iele_mul_ucd, ie_mul_ucd,                 &
     &      num_fld_mul_ucd(i_ucd), num_comp_mul_ucd(i_ucd),            &
     &      ncomp_mul_ucd(ist_ucd), fld_name_mul_ucd(ist_ucd),          &
     &      dat_mul_ucd(1,1) )
      end do
!
      end subroutine output_multi_ucds
!
! ----------------------------------------------------------------------
!
      end module output_multi_ucd
