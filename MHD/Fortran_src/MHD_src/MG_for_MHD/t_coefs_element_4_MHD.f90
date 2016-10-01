!t_coefs_element_4_MHD.f90
!     module t_coefs_element_4_MHD
!
!        programmed by H.Matsui on Dec., 2008
!
!       subroutine alloc_ref_temp_type(numnod, ref_fld)
!       subroutine alloc_ext_magne_type(numnod, ref_fld)
!
!       subroutine dealloc_ref_temp_type(ref_fld)
!       subroutine dealloc_ext_magne_type(ref_fld)
!
      module t_coefs_element_4_MHD
!
      use m_precision
      use t_material_property
!
      implicit none
!
      type reference_field_MHD
        real(kind = kreal), allocatable :: ref_temp(:)
        real(kind = kreal), allocatable :: ext_magne(:,:)
      end type reference_field_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine alloc_ref_temp_type(numnod, ref_fld)
!
       integer(kind = kint), intent(in) :: numnod
       type(reference_field_MHD), intent(inout) :: ref_fld
!
       allocate( ref_fld%ref_temp(numnod) )
       if (numnod.gt.0) ref_fld%ref_temp = 1.0d0
!
       end subroutine alloc_ref_temp_type
!
! ----------------------------------------------------------------------
!
       subroutine alloc_ext_magne_type(numnod, ref_fld)
!
       integer(kind = kint), intent(in) :: numnod
       type(reference_field_MHD), intent(inout) :: ref_fld
!
       allocate( ref_fld%ext_magne(numnod,3) )
       if (numnod.gt.0) ref_fld%ext_magne = 1.0d0
!
       end subroutine alloc_ext_magne_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine dealloc_ref_temp_type(ref_fld)
!
       type(reference_field_MHD), intent(inout) :: ref_fld
!
       deallocate( ref_fld%ref_temp )
!
       end subroutine dealloc_ref_temp_type
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_ext_magne_type(ref_fld)
!
       type(reference_field_MHD), intent(inout) :: ref_fld
!
       deallocate( ref_fld%ext_magne )
!
       end subroutine dealloc_ext_magne_type
!
! ----------------------------------------------------------------------
!
       end module t_coefs_element_4_MHD
