!t_geometry_data_MHD.f90
!      module t_geometry_data_MHD
!
!       Written by H. Matsui on Dec., 2008
!
!> @brief structur for node and element list for MHD dynamo
!
!!      subroutine count_smp_size_4_area(area)
!!      subroutine count_empty_smp_area(area)
!
!       subroutine allocate_field_nod_list(fld)
!       subroutine allocate_field_ele_list(fld)
!       subroutine allocate_geometry_field_smp(fld)
!
!       subroutine dealloc_type_ele_connect_org(mesh_MHD)
!       subroutine deallocate_field_nod_list(fld)
!       subroutine deallocate_field_ele_list(fld)
!       subroutine deallocate_geometry_field_smp(fld)
!
!      subroutine alloc_ele_connect_org_type(ele, MHD_mesh)
!      subroutine dealloc_ele_connect_org_type(MHD_mesh)
!
      module t_geometry_data_MHD
!
      use m_precision
      use m_constants
!
      use t_comm_table
!
      implicit  none
!
!
      type field_geometry_data
!     number of node on this PE   (include external node)
        integer( kind=kint )  ::  numnod_fld
!     number of internal node on this PE
        integer( kind=kint )  ::  internal_node_fld
!     number of element on this PE (include external element)
        integer( kind=kint )  ::  numele_fld
!     start element ID
        integer( kind=kint )  ::  iele_start_fld
!     end element ID
        integer( kind=kint )  ::  iele_end_fld
!
!     node table for field
        integer(kind=kint), pointer :: inod_fld(:)
!     element table for field
        integer(kind=kint), pointer :: iele_fld(:)
!
!     stack element for smp
        integer( kind=kint ), pointer :: istack_ele_fld_smp(:)
!     stack node for smp
        integer( kind=kint ), pointer :: istack_nod_fld_smp(:)
!     stack internal node for smp
        integer( kind=kint ), pointer :: istack_inter_fld_smp(:)
!
        real(kind=kreal) :: volume
!>     Area Volume
        real(kind=kreal) :: a_volume
!>     1 / (Area Volume)
      end type field_geometry_data
!
!
      type mesh_data_MHD
        type(communication_table) :: nod_fl_comm
        type(communication_table) :: nod_cd_comm
        type(communication_table) :: nod_ins_comm
!
        type(communication_table) :: ele_fl_comm
        type(communication_table) :: ele_cd_comm
        type(communication_table) :: ele_ins_comm
!
        type(communication_table) :: surf_fl_comm
        type(communication_table) :: surf_cd_comm
        type(communication_table) :: surf_ins_comm
!
        type(communication_table) :: edge_fl_comm
        type(communication_table) :: edge_cd_comm
        type(communication_table) :: edge_ins_comm
!
!>   geometry parameter for fluid
        type(field_geometry_data) :: fluid
!>   geometry parameter for conductor
        type(field_geometry_data) :: conduct
!>   geometry parameter for insulator
        type(field_geometry_data) :: insulate
!>   geometry parameter for inner core
        type(field_geometry_data) :: inner_core
!
!>       global element id (where i:element id)
        integer(kind=kint_gl), pointer :: iele_global_org(:)
!>     original element connectivity  (where i:nodal order j:element id)
        integer(kind=kint), pointer :: ie_org(:,:)
      end type mesh_data_MHD
!
      private :: allocate_geometry_field_smp
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_smp_size_4_area(area)
!
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      type(field_geometry_data), intent(inout) :: area
!
      integer( kind=kint )  ::  maxnod_fld_smp, max_in_nod_fld_smp
      integer( kind=kint )  ::  maxele_fld_smp
!
!
      call allocate_geometry_field_smp(area)
!
      call count_number_4_smp(np_smp, ione, area%numnod_fld,            &
     &     area%istack_nod_fld_smp, maxnod_fld_smp)
!
      call count_number_4_smp(np_smp, ione, area%internal_node_fld,     &
     &     area%istack_inter_fld_smp, max_in_nod_fld_smp)
!
      call count_number_4_smp                                           &
     &    (np_smp, area%iele_start_fld, area%iele_end_fld,              &
     &     area%istack_ele_fld_smp, maxele_fld_smp)
!
      end subroutine count_smp_size_4_area
!
!-----------------------------------------------------------------------
!
      subroutine count_empty_smp_area(area)
!
      type(field_geometry_data), intent(inout) :: area
!
!
      call allocate_geometry_field_smp(area)
!
      area%istack_nod_fld_smp = 0
      area%istack_inter_fld_smp = 0
      area%istack_ele_fld_smp = 0
!
      end subroutine count_empty_smp_area
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine allocate_field_nod_list(fld)
!
       type(field_geometry_data), intent(inout) :: fld
!
       allocate(fld%inod_fld(fld%numnod_fld))
       if(fld%numnod_fld .gt. 0) fld%inod_fld = 0
!
       end subroutine allocate_field_nod_list
!
! ----------------------------------------------------------------------
!
       subroutine allocate_field_ele_list(fld)
!
       type(field_geometry_data), intent(inout) :: fld
!
       allocate(fld%iele_fld(fld%numele_fld))
       if(fld%numele_fld .gt. 0) fld%iele_fld = 0
!
       end subroutine allocate_field_ele_list
!
! ----------------------------------------------------------------------
!
       subroutine allocate_geometry_field_smp(fld)
!
       use m_machine_parameter
!
       type(field_geometry_data), intent(inout) :: fld
!
       allocate( fld%istack_ele_fld_smp(0:np_smp))
       allocate( fld%istack_nod_fld_smp(0:np_smp))
       allocate( fld%istack_inter_fld_smp(0:np_smp))
!
       fld%istack_ele_fld_smp =   0
       fld%istack_nod_fld_smp =   0
       fld%istack_inter_fld_smp = 0
!
       end subroutine allocate_geometry_field_smp
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine deallocate_field_nod_list(fld)
!
       type(field_geometry_data), intent(inout) :: fld
!
       deallocate(fld%inod_fld)
!
       end subroutine deallocate_field_nod_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_field_ele_list(fld)
!
       type(field_geometry_data), intent(inout) :: fld
!
       deallocate(fld%iele_fld)
!
       end subroutine deallocate_field_ele_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_geometry_field_smp(fld)
!
       type(field_geometry_data), intent(inout) :: fld
!
       deallocate( fld%istack_ele_fld_smp)
       deallocate( fld%istack_nod_fld_smp)
       deallocate( fld%istack_inter_fld_smp)
!
       end subroutine deallocate_geometry_field_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_ele_connect_org_type(ele, MHD_mesh)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      allocate(MHD_mesh%iele_global_org(ele%numele))
      allocate(MHD_mesh%ie_org(ele%numele,ele%nnod_4_ele))
      MHD_mesh%iele_global_org = 0
      MHD_mesh%ie_org =          0
!
      end subroutine alloc_ele_connect_org_type
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_connect_org_type(MHD_mesh)
!
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      deallocate(MHD_mesh%iele_global_org)
      deallocate(MHD_mesh%ie_org)
!
      end subroutine dealloc_ele_connect_org_type
!
! ----------------------------------------------------------------------
!
      end module t_geometry_data_MHD
