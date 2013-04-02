!t_coefs_element_4_MHD.f90
!     module t_coefs_element_4_MHD
!
!        programmed by H.Matsui on Dec., 2008
!
!       subroutine alloc_velo_diff_MHD_AMG(numele, ak_AMG)
!       subroutine alloc_temp_diff_MHD_AMG(numele, ak_AMG)
!       subroutine alloc_magne_diff_MHD_AMG(numele, ak_AMG)
!       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_AMG)
!       subroutine alloc_diff_coefs_velo_type(numele, ak_AMG)
!       subroutine alloc_diff_coefs_temp_type(numele, ak_AMG)
!       subroutine alloc_diff_coefs_magne_type(numele, ak_AMG)
!       subroutine alloc_diff_coefs_ds_type(numele, ak_AMG)
!       subroutine alloc_ref_temp_type(numnod, ref_fld)
!       subroutine alloc_ext_magne_type(numnod, ref_fld)
!
!       subroutine dealloc_velo_diff_MHD_AMG(ak_AMG)
!       subroutine dealloc_temp_diff_MHD_AMG(ak_AMG)
!       subroutine dealloc_magne_diff_MHD_AMG(ak_AMG)
!       subroutine dealloc_dscalar_diff_MHD_AMG(ak_AMG)
!       subroutine dealloc_diff_coefs_velo_type(ak_AMG)
!       subroutine dealloc_diff_coefs_temp_type(ak_AMG)
!       subroutine dealloc_diff_coefs_magne_type(ak_AMG)
!       subroutine dealloc_diff_coefs_ds_type(ak_AMG)
!       subroutine dealloc_ref_temp_type(ref_fld)
!       subroutine dealloc_ext_magne_type(ref_fld)
!
      module t_coefs_element_4_MHD
!
      use m_precision
!
      implicit none
!
      type coefs_4_MHD_AMG
        real  (kind=kreal), pointer :: ak_d_velo(:)
        real  (kind=kreal), pointer :: ak_d_temp(:)
        real  (kind=kreal), pointer :: ak_d_magne(:)
        real  (kind=kreal), pointer :: ak_d_composit(:)
!
        real  (kind=kreal), pointer :: ak_diff_v(:)
        real  (kind=kreal), pointer :: ak_diff_t(:)
        real  (kind=kreal), pointer :: ak_diff_b(:)
        real  (kind=kreal), pointer :: ak_diff_d(:)
      end type coefs_4_MHD_AMG
! 
      type reference_field_MHD
        real  (kind=kreal), pointer :: ref_temp(:)
        real  (kind=kreal), pointer :: ext_magne(:,:)
      end type reference_field_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine alloc_velo_diff_MHD_AMG(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_d_velo(numele) )
       if (numele.gt.0) ak_AMG%ak_d_velo = 0.0d0
!
       end subroutine alloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_temp_diff_MHD_AMG(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_d_temp(numele) )
       if (numele.gt.0) ak_AMG%ak_d_temp = 0.0d0
!
       end subroutine alloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_magne_diff_MHD_AMG(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_d_magne(numele) )
       if (numele.gt.0) ak_AMG%ak_d_magne = 0.0d0
!
       end subroutine alloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_d_composit(numele) )
       if (numele.gt.0) ak_AMG%ak_d_composit = 0.0d0
!
       end subroutine alloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine alloc_diff_coefs_velo_type(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_diff_v(numele) )
       if (numele.gt.0) ak_AMG%ak_diff_v = 1.0d0
!
       end subroutine alloc_diff_coefs_velo_type
!
! ----------------------------------------------------------------------
!
       subroutine alloc_diff_coefs_temp_type(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_diff_t(numele) )
       if (numele.gt.0) ak_AMG%ak_diff_t = 1.0d0
!
       end subroutine alloc_diff_coefs_temp_type
!
! ----------------------------------------------------------------------
!
       subroutine alloc_diff_coefs_magne_type(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_diff_b(numele) )
       if (numele.gt.0) ak_AMG%ak_diff_b = 1.0d0
!
       end subroutine alloc_diff_coefs_magne_type
!
! ----------------------------------------------------------------------
!
       subroutine alloc_diff_coefs_ds_type(numele, ak_AMG)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       allocate( ak_AMG%ak_diff_d(numele) )
       if (numele.gt.0) ak_AMG%ak_diff_d = 1.0d0
!
       end subroutine alloc_diff_coefs_ds_type
!
! ----------------------------------------------------------------------
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
       subroutine dealloc_velo_diff_MHD_AMG(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_d_velo )
!
       end subroutine dealloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_temp_diff_MHD_AMG(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_d_temp )
!
       end subroutine dealloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_magne_diff_MHD_AMG(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_d_magne )
!
       end subroutine dealloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_dscalar_diff_MHD_AMG(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_d_composit )
!
       end subroutine dealloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine dealloc_diff_coefs_velo_type(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_diff_v )
!
       end subroutine dealloc_diff_coefs_velo_type
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_diff_coefs_temp_type(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_diff_t )
!
       end subroutine dealloc_diff_coefs_temp_type
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_diff_coefs_magne_type(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_diff_b )
!
       end subroutine dealloc_diff_coefs_magne_type
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_diff_coefs_ds_type(ak_AMG)
!
       type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
       deallocate( ak_AMG%ak_diff_d )
!
       end subroutine dealloc_diff_coefs_ds_type
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
