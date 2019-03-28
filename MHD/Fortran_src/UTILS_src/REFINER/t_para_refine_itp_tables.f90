!t_para_refine_itp_tables.f90
!      module t_para_refine_itp_tables
!
!      Written by H. Matsui on May., 2010
!
!!      subroutine alloc_para_fine_mesh_type(para_ref_itp)
!!      subroutine alloc_para_course_mesh_type(para_ref_itp)
!!      subroutine alloc_para_refine_itp_type(para_ref_itp)
!!        type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!!
!!      subroutine dealloc_para_fine_mesh_type(para_ref_itp)
!!      subroutine dealloc_para_course_mesh_type(para_ref_itp)
!!      subroutine dealloc_para_refine_itp_type(para_ref_itp)
!!        type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      module t_para_refine_itp_tables
!
      use m_precision
!
      use t_mesh_data
      use t_interpolate_table
!
!
      implicit  none
!
!
      type para_refine_itp_tables
        integer :: nprocs_fine = 1
        type(mesh_data), allocatable :: fine_mesh(:)
!
        integer :: nprocs_course = 1
        type(mesh_data), allocatable :: course_mesh(:)
!
        integer :: nprocs_larger = 1
        type(interpolate_table), allocatable :: c2f_para(:)
        type(interpolate_table), allocatable :: f2c_para(:)
        type(interpolate_table), allocatable :: f2c_ele_para(:)
!
        type(interpolate_table) :: c2f_single
        type(interpolate_table) :: f2c_single
        type(interpolate_table) :: f2c_ele_single
      end type para_refine_itp_tables
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_fine_mesh_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      allocate(para_ref_itp%fine_mesh(para_ref_itp%nprocs_fine))
!
      end subroutine alloc_para_fine_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_course_mesh_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      allocate(para_ref_itp%course_mesh(para_ref_itp%nprocs_course))
!
      end subroutine alloc_para_course_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_refine_itp_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      allocate(para_ref_itp%c2f_para(para_ref_itp%nprocs_larger))
      allocate(para_ref_itp%f2c_para(para_ref_itp%nprocs_larger))
      allocate(para_ref_itp%f2c_ele_para(para_ref_itp%nprocs_larger))
!
      end subroutine alloc_para_refine_itp_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_fine_mesh_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      deallocate(para_ref_itp%fine_mesh)
!
      end subroutine dealloc_para_fine_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_course_mesh_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      deallocate(para_ref_itp%course_mesh)
!
      end subroutine dealloc_para_course_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_refine_itp_type(para_ref_itp)
!
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      deallocate(para_ref_itp%c2f_para)
      deallocate(para_ref_itp%f2c_para)
      deallocate(para_ref_itp%f2c_ele_para)
!
      end subroutine dealloc_para_refine_itp_type
!
! -----------------------------------------------------------------------
!
      end module t_para_refine_itp_tables
