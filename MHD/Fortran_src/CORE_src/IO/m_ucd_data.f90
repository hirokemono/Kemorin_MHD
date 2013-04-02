!
!      module m_ucd_data
!
!        Written by H.Matsui on June, 2006
!
!      subroutine allocate_ucd_node
!      subroutine allocate_ucd_ele
!      subroutine allocate_ucd_phys_name
!      subroutine allocate_ucd_phys_data
!
!      subroutine deallocate_ucd_node
!      subroutine deallocate_ucd_ele
!      subroutine deallocate_ucd_phys_data
!      subroutine deallocate_ucd_phys_name
!      subroutine deallocate_ucd_data
!
!      subroutine disconnect_ucd_node
!      subroutine disconnect_ucd_data
!
!      subroutine cal_istack_ucd_component
!
      module m_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      implicit none
!
      integer (kind = kint), parameter :: ucd_file_code = 16
      character(len=kchara) :: ucd_file_name
!
!
      character(len=kchara) :: ucd_header_name = "field/out"
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!
      integer (kind = kint) :: itype_ucd_data_file = iflag_fld
!  data file type of field data
!
      integer(kind = kint) :: nnod_ucd, nele_ucd
      integer(kind = kint) :: nnod_4_ele_ucd
!
      real (kind=kreal), pointer :: xx_ucd(:,:)
      integer(kind = kint), pointer :: inod_gl_ucd(:)
      integer(kind = kint), pointer :: iele_gl_ucd(:)
      integer(kind = kint), pointer :: ie_ucd(:,:)
      character (len=5) :: hexmark = ' hex '
      character (len=5) :: trimark = ' tri '
!
!
      integer(kind=kint) :: num_field_ucd
      integer(kind=kint) :: ntot_comp_ucd
      integer(kind=kint), pointer :: num_comp_ucd(:)
      integer(kind=kint), pointer :: istack_comp_ucd(:)
      character (len=kchara), pointer :: phys_name_ucd(:)
!
      real (kind=kreal), pointer :: d_nod_ucd(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_node
!
      allocate(xx_ucd(nnod_ucd,3))
      allocate(inod_gl_ucd(nnod_ucd))
!
      if(nnod_ucd .gt. 0) then
        xx_ucd = 0.0d0
        inod_gl_ucd = 0
      end if
!
      end subroutine allocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_ele
!
      allocate(ie_ucd(nele_ucd,nnod_4_ele_ucd))
      allocate(iele_gl_ucd(nele_ucd))
!
      if(nele_ucd .gt. 0) then
        ie_ucd = 0
        iele_gl_ucd =   0
      end if
!
      end subroutine allocate_ucd_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_name
!
      allocate( num_comp_ucd(num_field_ucd) )
      allocate( istack_comp_ucd(0:num_field_ucd) )
      allocate( phys_name_ucd(num_field_ucd) )
!
      istack_comp_ucd = 0
      if(num_field_ucd .gt. 0) num_comp_ucd = 0
!
      end subroutine allocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_data
!
      allocate(d_nod_ucd(nnod_ucd,ntot_comp_ucd) )
      if( (nnod_ucd*ntot_comp_ucd) .gt. 0) d_nod_ucd = 0.0d0
!
      end subroutine allocate_ucd_phys_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_node
!
      deallocate(xx_ucd)
      deallocate(inod_gl_ucd)
!
      end subroutine deallocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_ele
!
      deallocate(ie_ucd)
      deallocate(iele_gl_ucd)
!
      end subroutine deallocate_ucd_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_data
!
      deallocate(d_nod_ucd)
!
      end subroutine deallocate_ucd_phys_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_name
!
      deallocate(num_comp_ucd, istack_comp_ucd)
      deallocate(phys_name_ucd)
!
      end subroutine deallocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_data
!
      call deallocate_ucd_phys_name
      call deallocate_ucd_phys_data
!
      end subroutine deallocate_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_node
!
      nullify(xx_ucd)
      nullify(inod_gl_ucd)
!
      end subroutine disconnect_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_data
!
      nullify(num_comp_ucd, istack_comp_ucd)
      nullify(phys_name_ucd)
      nullify(d_nod_ucd)
!
      end subroutine disconnect_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_istack_ucd_component
!
      use m_constants
      use cal_minmax_and_stacks
!
!
      call s_cal_total_and_stacks(num_field_ucd, num_comp_ucd,          &
     &    izero, istack_comp_ucd, ntot_comp_ucd)
!
      end subroutine cal_istack_ucd_component
!
! -------------------------------------------------------------------
!
      end module m_ucd_data
