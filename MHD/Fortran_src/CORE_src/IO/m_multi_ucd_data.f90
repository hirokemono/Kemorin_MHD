!
!      module m_multi_ucd_data
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_multl_ucd_params
!      subroutine allocate_multl_ucd_labels
!      subroutine allocate_multl_ucd_grids
!      subroutine allocate_multl_ucd_fields
!
!      subroutine deallocate_multl_ucd_data
!      subroutine unlink_multl_ucd_data
!
      module m_multi_ucd_data
!
      use m_precision
      use m_geometry_constants
!
      implicit  none
!
      integer(kind = kint) :: num_mul_ucd
      character(len = kchara), pointer :: mul_ucd_header(:)
!
      integer(kind = kint), pointer :: itype_mul_ucd_file(:)
      integer(kind = kint), parameter :: id_type_UCD_file = 0
      integer(kind = kint), parameter :: id_type_UDT_file = 1
!
!
      integer(kind = kint), pointer :: num_fld_mul_ucd(:)
      integer(kind = kint), pointer :: istack_fld_mul_ucd(:)
      integer(kind = kint), pointer :: num_comp_mul_ucd(:)
!
      integer(kind = kint) :: ntot_fld_mul_ucd_out
      integer(kind = kint), pointer :: ncomp_mul_ucd(:)
      character(len = kchara), pointer :: fld_name_mul_ucd(:)
!
      integer(kind = kint), pointer :: istack_nod_mul_ucd(:)
      integer(kind = kint), pointer :: istack_ele_mul_ucd(:)
!
      integer(kind = kint) :: ntot_nod_mul_ucd = 0
      integer(kind = kint), pointer :: inod_mul_ucd(:)
      real(kind = kreal), pointer :: xx_mul_ucd(:,:)
!
      integer(kind = kint) :: ntot_ele_mul_ucd = 0
      integer(kind = kint) :: nnod_4_ele_mul_ucd = 0
      integer(kind = kint), pointer :: iele_mul_ucd(:)
      integer(kind = kint), pointer :: ie_mul_ucd(:,:)
!
      integer(kind = kint) :: max_comp_mul_ucd
      real(kind = kreal), pointer :: dat_mul_ucd(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_multl_ucd_params
!
!
      allocate(mul_ucd_header(num_mul_ucd))
      allocate(itype_mul_ucd_file(num_mul_ucd))
!
      allocate(num_fld_mul_ucd(num_mul_ucd))
      allocate(istack_fld_mul_ucd(0:num_mul_ucd))
      allocate(num_comp_mul_ucd(num_mul_ucd))
!
      allocate( istack_nod_mul_ucd(0:num_mul_ucd) )
      allocate( istack_ele_mul_ucd(0:num_mul_ucd) )
!
      if(num_mul_ucd .gt. 0) then
        itype_mul_ucd_file = 0
!
        num_fld_mul_ucd = 0
        num_comp_mul_ucd = 0
      end if
!
      istack_fld_mul_ucd = 0
      istack_nod_mul_ucd = 0
      istack_ele_mul_ucd = 0
!
      end subroutine allocate_multl_ucd_params
!
! ----------------------------------------------------------------------
!
      subroutine allocate_multl_ucd_labels
!
!
      allocate(ncomp_mul_ucd(ntot_fld_mul_ucd_out))
      allocate(fld_name_mul_ucd(ntot_fld_mul_ucd_out))
!
      if(num_mul_ucd .gt. 0)  ncomp_mul_ucd = 0
!
      end subroutine allocate_multl_ucd_labels
!
! ----------------------------------------------------------------------
!
      subroutine allocate_multl_ucd_grids
!
!
      allocate(inod_mul_ucd(ntot_nod_mul_ucd))
      allocate(xx_mul_ucd(ntot_nod_mul_ucd,3))
!
      allocate(iele_mul_ucd(ntot_ele_mul_ucd))
      allocate(ie_mul_ucd(ntot_ele_mul_ucd,nnod_4_ele_mul_ucd))
!
      if(ntot_nod_mul_ucd .gt. 0)  then
        inod_mul_ucd = 0
        xx_mul_ucd = 0.0d0
      end if
      if(ntot_ele_mul_ucd .gt. 0)  then
        iele_mul_ucd = 0
        ie_mul_ucd =   0
      end if
!
      end subroutine allocate_multl_ucd_grids
!
! ----------------------------------------------------------------------
!
      subroutine allocate_multl_ucd_fields
!
!
      allocate(dat_mul_ucd(ntot_nod_mul_ucd,max_comp_mul_ucd))
      if(ntot_nod_mul_ucd .gt. 0) dat_mul_ucd = 0.0d0
!
      end subroutine allocate_multl_ucd_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_multl_ucd_data
!
!
      deallocate(mul_ucd_header)
      deallocate(itype_mul_ucd_file)
!
      deallocate(num_fld_mul_ucd, istack_fld_mul_ucd, num_comp_mul_ucd)
!
      deallocate( istack_nod_mul_ucd, istack_ele_mul_ucd )
!
      deallocate(ncomp_mul_ucd, fld_name_mul_ucd)
      deallocate(inod_mul_ucd, xx_mul_ucd)
      deallocate(iele_mul_ucd, ie_mul_ucd)
!
      deallocate(dat_mul_ucd)
!
      end subroutine deallocate_multl_ucd_data
!
! ----------------------------------------------------------------------
!
      subroutine unlink_multl_ucd_data
!
!
      nullify(mul_ucd_header)
      nullify(itype_mul_ucd_file)
!
      nullify(num_fld_mul_ucd, istack_fld_mul_ucd, num_comp_mul_ucd)
!
      nullify( istack_nod_mul_ucd, istack_ele_mul_ucd )
!
      nullify(ncomp_mul_ucd, fld_name_mul_ucd)
      nullify(inod_mul_ucd, xx_mul_ucd)
      nullify(iele_mul_ucd, ie_mul_ucd)
!
      nullify(dat_mul_ucd)
!
      end subroutine unlink_multl_ucd_data
!
! ----------------------------------------------------------------------
!
      end module m_multi_ucd_data
