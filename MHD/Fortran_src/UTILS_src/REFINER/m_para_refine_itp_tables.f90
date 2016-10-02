!m_para_refine_itp_tables.f90
!      module m_para_refine_itp_tables
!
      module m_para_refine_itp_tables
!
!      Written by H. Matsui on May., 2010
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
      integer (kind = kint) :: nprocs_fine = 1
      type(mesh_data), pointer :: fine_mesh(:)
!
      integer (kind = kint) :: nprocs_course = 1
      type(mesh_data), pointer :: course_mesh(:)
!
      integer (kind = kint) :: nprocs_larger = 1
      type(interpolate_table), pointer :: c2f_para(:)
      type(interpolate_table), pointer :: f2c_para(:)
      type(interpolate_table), pointer :: f2c_ele_para(:)
!
      type(interpolate_table) :: c2f_single
      type(interpolate_table) :: f2c_single
      type(interpolate_table) :: f2c_ele_single
!
!      subroutine alloc_para_fine_mesh_type
!      subroutine alloc_para_course_mesh_type
!      subroutine alloc_para_refine_itp_type
!
!      subroutine dealloc_para_fine_mesh_type
!      subroutine dealloc_para_course_mesh_type
!      subroutine dealloc_para_refine_itp_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_fine_mesh_type
!
!
      allocate( fine_mesh(nprocs_fine) )
!
      end subroutine alloc_para_fine_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_course_mesh_type
!
!
      allocate( course_mesh(nprocs_course) )
!
      end subroutine alloc_para_course_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_para_refine_itp_type
!
!
      allocate( c2f_para(nprocs_larger) )
      allocate( f2c_para(nprocs_larger) )
      allocate( f2c_ele_para(nprocs_larger) )
!
      end subroutine alloc_para_refine_itp_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_fine_mesh_type
!
!
      deallocate( fine_mesh )
!
      end subroutine dealloc_para_fine_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_course_mesh_type
!
!
      deallocate( course_mesh )
!
      end subroutine dealloc_para_course_mesh_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_para_refine_itp_type
!
!
      deallocate( c2f_para )
      deallocate( f2c_para )
      deallocate( f2c_ele_para )
!
      end subroutine dealloc_para_refine_itp_type
!
! -----------------------------------------------------------------------
!
      end module m_para_refine_itp_tables
