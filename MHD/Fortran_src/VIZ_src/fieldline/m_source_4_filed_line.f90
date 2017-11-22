!m_source_4_filed_line.f90
!
!      module m_source_4_filed_line
!
!      Written by H. Matsui on Aug., 2011
!
!      subroutine allocate_local_start_grp_num
!      subroutine allocate_local_start_grp_item
!      subroutine allocate_local_data_4_fline(numnod)
!      subroutine allocate_start_point_fline
!      subroutine deallocate_local_data_4_fline
!      subroutine deallocate_local_start_grp_item
!      subroutine deallocate_start_point_fline
!
      module m_source_4_filed_line
!
      use m_precision
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: vector_nod_fline(:,:,:)
      real(kind = kreal), allocatable :: color_nod_fline(:,:)
!
      integer(kind = kint) :: ntot_ele_start_grp
      integer(kind = kint), allocatable :: istack_ele_start_grp(:)
      integer(kind = kint), allocatable :: nele_start_grp(:)
      integer(kind = kint), allocatable :: iele_start_item(:,:)
      real(kind = kreal),   allocatable :: flux_start(:)
!
      integer(kind = kint), allocatable :: num_line_local(:)
!
      real(kind = kreal),   allocatable :: xx_start_fline(:,:)
      real(kind = kreal),   allocatable :: flux_start_fline(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_start_grp_num
!
      use m_control_params_4_fline
!
!
      allocate(nele_start_grp(num_fline))
      allocate(istack_ele_start_grp(0:num_fline))
!
      allocate(num_line_local(num_fline))
!
      nele_start_grp =       0
      istack_ele_start_grp = 0
!
      num_line_local = 0
!
      end subroutine allocate_local_start_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_start_grp_item
!
      use m_control_params_4_fline
!
!
      ntot_ele_start_grp = istack_ele_start_grp(num_fline)
      allocate(iele_start_item(2,ntot_ele_start_grp))
      allocate(flux_start(ntot_ele_start_grp))
      if(ntot_ele_start_grp .gt. 0) then
        iele_start_item =    0
      end if
!
      end subroutine allocate_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_data_4_fline(numnod)
!
      use m_control_params_4_fline
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(vector_nod_fline(numnod,3,num_fline))
      allocate(color_nod_fline(numnod,num_fline))
!
      vector_nod_fline = 0.0d0
      color_nod_fline =  0.0d0
!
      end subroutine allocate_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_start_point_fline
!
      use m_control_params_4_fline
!
!
      allocate(xx_start_fline(3,ntot_each_field_line))
      allocate(flux_start_fline(ntot_each_field_line))
!
      xx_start_fline = 0.0d0
      flux_start_fline =  0.0d0
!
      end subroutine allocate_start_point_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_start_grp_item
!
      deallocate(nele_start_grp, istack_ele_start_grp)
      deallocate(iele_start_item, flux_start)
      deallocate(num_line_local)
!
      end subroutine deallocate_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_data_4_fline
!
      deallocate(vector_nod_fline, color_nod_fline)
!
      end subroutine deallocate_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_start_point_fline
!
!
      deallocate(xx_start_fline, flux_start_fline)
!
      end subroutine deallocate_start_point_fline
!
!  ---------------------------------------------------------------------
!
      end module m_source_4_filed_line
