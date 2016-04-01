!m_SGS_model_coefs.f90
!     module m_SGS_model_coefs
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!       subroutine allocate_model_coefs(numele)
!       subroutine allocate_nod_model_coefs(numnod)
!
!       subroutine deallocate_model_coefs
!       subroutine deallocate_nod_model_coefs
!
      module m_SGS_model_coefs
!
      use m_precision
      use t_material_property
!
      implicit  none
!
!
      type(MHD_coefficients_type), save :: sgs_coefs
!
      type(MHD_coefficients_type), save :: diff_coefs
!diff_coefs%ak
!
      real  (kind=kreal), allocatable :: ak_sgs_nod(:,:)
!
!      real  (kind=kreal), allocatable :: ak_diff(:,:)
!
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_model_coefs(numele)
!
      integer(kind = kint), intent(in) :: numele
!
!
      call alloc_MHD_num_coefs(sgs_coefs)
      call alloc_MHD_coefs(numele, sgs_coefs)
!
       allocate( diff_coefs%num_comps(diff_coefs%num_field) )
       allocate( diff_coefs%istack_comps(0:diff_coefs%num_field) )
       allocate( diff_coefs%iflag_field(diff_coefs%num_field) )
       allocate( diff_coefs%ak(numele,diff_coefs%num_field) )
!
       if (diff_coefs%num_field .gt. 0) then
         diff_coefs%num_comps =  0
         diff_coefs%iflag_field = 0
         diff_coefs%ak = 1.0d0
       end if
       diff_coefs%istack_comps = 0
!
       end subroutine allocate_model_coefs
!
!  --------------------------------------------------------------------
!
      subroutine allocate_nod_model_coefs(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( ak_sgs_nod(numnod,sgs_coefs%ntot_comp) )
!
      if (sgs_coefs%num_field .gt. 0) ak_sgs_nod =  1.0d0 
!
      end subroutine allocate_nod_model_coefs
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
       subroutine deallocate_model_coefs
!
!
      call dealloc_MHD_coefs(sgs_coefs)
       deallocate( diff_coefs%num_comps )
       deallocate( diff_coefs%istack_comps )
       deallocate( diff_coefs%iflag_field )
       deallocate( diff_coefs%ak )
!
       end subroutine deallocate_model_coefs
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_nod_model_coefs
!
!
       deallocate( ak_sgs_nod )
!
       end subroutine deallocate_nod_model_coefs
!
!  --------------------------------------------------------------------
!
      end module m_SGS_model_coefs
