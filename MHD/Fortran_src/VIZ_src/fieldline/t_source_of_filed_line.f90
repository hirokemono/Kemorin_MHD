!t_source_of_filed_line.f90
!
!      module t_source_of_filed_line
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine alloc_local_start_grp_num(fline_src)
!!      subroutine alloc_local_start_grp_item(fline_src)
!!      subroutine alloc_local_data_4_fline(numnod, fline_src)
!!      subroutine alloc_start_point_fline(fline_src)
!!      subroutine alloc_num_gl_start_fline                             &
!!     &         (nprocs, num_fline, ntot_each_field_line, fline_tce)
!!      subroutine dealloc_local_data_4_fline(fline_src)
!!      subroutine dealloc_local_start_grp_item(fline_src)
!!      subroutine dealloc_start_point_fline(fline_src)
!!      subroutine dealloc_num_gl_start_fline(fline_tce)
!
      module t_source_of_filed_line
!
      use m_precision
!
      implicit  none
!
!
      type fieldline_source
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
      end type fieldline_source
!
!
      type fieldline_trace
        integer(kind = kint) :: ntot_gl_fline = 0
!
        integer(kind = kint), allocatable :: istack_all_fline(:,:)
        integer(kind = kint), allocatable :: num_all_fline(:,:)
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!
        integer(kind= kint), allocatable :: iflag_fline(:)
        integer(kind= kint), allocatable :: icount_fline(:)
        integer(kind= kint), allocatable :: isf_fline_start(:,:)
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
        real(kind = kreal), allocatable ::  v_fline_start(:,:)
        real(kind = kreal), allocatable ::  c_fline_start(:)
!
        integer(kind= kint), allocatable :: id_fline_export(:,:)
        real(kind = kreal), allocatable ::  fline_export(:,:)
!
        integer(kind= kint), allocatable :: isf_fline_global(:,:)
        real(kind = kreal), allocatable ::  fline_global(:,:)
      end type fieldline_trace
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_num(fline_src)
!
      use m_control_params_4_fline
!
      type(fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%nele_start_grp(num_fline))
      allocate(fline_src%istack_ele_start_grp(0:num_fline))
!
      allocate(fline_src%num_line_local(num_fline))
!
      fline_src%nele_start_grp =       0
      fline_src%istack_ele_start_grp = 0
!
      fline_src%num_line_local = 0
!
      end subroutine alloc_local_start_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(fline_src)
!
      use m_control_params_4_fline
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: num
!
!
      fline_src%ntot_ele_start_grp                                      &
     &            = fline_src%istack_ele_start_grp(num_fline)
      num = fline_src%ntot_ele_start_grp
!
      allocate(fline_src%iele_start_item(2,num))
      allocate(fline_src%flux_start(num))
      if(num .gt. 0) fline_src%iele_start_item = 0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_data_4_fline(numnod, fline_src)
!
      use m_control_params_4_fline
!
      integer(kind = kint), intent(in) :: numnod
      type(fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%vector_nod_fline(numnod,3,num_fline))
      allocate(fline_src%color_nod_fline(numnod,num_fline))
!
      fline_src%vector_nod_fline = 0.0d0
      fline_src%color_nod_fline =  0.0d0
!
      end subroutine alloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_start_point_fline(fline_src)
!
      use m_control_params_4_fline
!
      type(fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%xx_start_fline(3,ntot_each_field_line))
      allocate(fline_src%flux_start_fline(ntot_each_field_line))
!
      fline_src%xx_start_fline = 0.0d0
      fline_src%flux_start_fline =  0.0d0
!
      end subroutine alloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_gl_start_fline                               &
     &         (nprocs, num_fline, ntot_each_field_line, fline_tce)
!
      integer(kind = kint), intent(in) :: num_fline, nprocs
      integer(kind = kint), intent(in) :: ntot_each_field_line
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: num
!
!
      allocate(fline_tce%istack_all_fline(0:nprocs,num_fline))
      allocate(fline_tce%num_all_fline(nprocs,num_fline))
      allocate(fline_tce%flux_stack_fline(0:nprocs))
!
      num = 2*ntot_each_field_line
      allocate(fline_tce%icount_fline(num))
      allocate(fline_tce%iflag_fline(num))
      allocate(fline_tce%isf_fline_start(3,num))
      allocate(fline_tce%xx_fline_start(3,num))
      allocate(fline_tce%v_fline_start(3,num))
      allocate(fline_tce%c_fline_start(num))
!
      fline_tce%ntot_gl_fline =    0
      fline_tce%istack_all_fline = 0
      fline_tce%num_all_fline =    0
      fline_tce%flux_stack_fline = 0.0d0
!
      fline_tce%icount_fline = 0
      fline_tce%iflag_fline =  0
      fline_tce%isf_fline_start = 0
      fline_tce%xx_fline_start = 0.0d0
      fline_tce%v_fline_start =  0.0d0
      fline_tce%c_fline_start =  0.0d0
!
      allocate(fline_tce%id_fline_export(7,num))
      allocate(fline_tce%fline_export(7,num))
      fline_tce%id_fline_export = 0
      fline_tce%fline_export = 0.0d0
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_start_grp_item(fline_src)
!
      type(fieldline_source), intent(inout) :: fline_src
!
!
      deallocate(fline_src%nele_start_grp)
      deallocate(fline_src%istack_ele_start_grp)
      deallocate(fline_src%iele_start_item, fline_src%flux_start)
      deallocate(fline_src%num_line_local)
!
      end subroutine dealloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_data_4_fline(fline_src)
!
      type(fieldline_source), intent(inout) :: fline_src
!
!
      deallocate(fline_src%vector_nod_fline, fline_src%color_nod_fline)
!
      end subroutine dealloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_start_point_fline(fline_src)
!
      type(fieldline_source), intent(inout) :: fline_src
!
      deallocate(fline_src%xx_start_fline, fline_src%flux_start_fline)
!
      end subroutine dealloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline(fline_tce)
!
      type(fieldline_trace), intent(inout) :: fline_tce
!
!
      deallocate(fline_tce%istack_all_fline)
      deallocate(fline_tce%num_all_fline)
      deallocate(fline_tce%flux_stack_fline)
!
      deallocate(fline_tce%icount_fline)
      deallocate(fline_tce%iflag_fline)
      deallocate(fline_tce%isf_fline_start)
      deallocate(fline_tce%xx_fline_start)
      deallocate(fline_tce%v_fline_start)
      deallocate(fline_tce%c_fline_start)
!
      deallocate(fline_tce%id_fline_export)
      deallocate(fline_tce%fline_export)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
