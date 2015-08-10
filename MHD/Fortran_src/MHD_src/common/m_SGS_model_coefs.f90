!m_SGS_model_coefs.f90
!     module m_SGS_model_coefs
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!       subroutine allocate_model_coefs
!       subroutine allocate_nod_model_coefs(numnod)
!
!       subroutine deallocate_model_coefs
!       subroutine deallocate_nod_model_coefs
!
      module m_SGS_model_coefs
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_sgs_coefs
      integer (kind=kint) :: num_sgs_kinds
      integer (kind=kint), allocatable  :: iflag_sgs_coefs(:)
      integer (kind=kint), allocatable  :: ncomp_sgs_coefs(:)
      integer (kind=kint), allocatable  :: istack_sgs_coefs(:)
      real  (kind=kreal), allocatable :: ak_sgs(:,:)
      real  (kind=kreal), allocatable :: ak_sgs_nod(:,:)
!
      integer (kind=kint) :: num_diff_coefs
      integer (kind=kint) :: num_diff_kinds
      integer (kind=kint), allocatable  :: iflag_diff_coefs(:)
      integer (kind=kint), allocatable  :: ncomp_diff_coefs(:)
      integer (kind=kint), allocatable  :: istack_diff_coefs(:)
      real  (kind=kreal), allocatable :: ak_diff(:,:)
      real  (kind=kreal), allocatable :: ak_diff_nod(:,:)
!
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
       subroutine allocate_model_coefs
!
       use m_geometry_parameter
!
       allocate( ncomp_sgs_coefs(num_sgs_kinds) )
       allocate( istack_sgs_coefs(0:num_sgs_kinds) )
       allocate( iflag_sgs_coefs(num_sgs_kinds) )
       allocate( ak_sgs(numele,num_sgs_coefs) )
!
       allocate( ncomp_diff_coefs(num_diff_kinds) )
       allocate( istack_diff_coefs(0:num_diff_kinds) )
       allocate( iflag_diff_coefs(num_diff_kinds) )
       allocate( ak_diff(numele,num_diff_kinds) )
!
       if (num_sgs_kinds .gt. 0) then
         ncomp_sgs_coefs =  0
         iflag_sgs_coefs =  0
         ak_sgs =  1.0d0
       end if
       istack_sgs_coefs = 0
!
       if (num_diff_kinds .gt. 0) then
         ncomp_diff_coefs =  0
         iflag_diff_coefs = 0
         ak_diff = 1.0d0
       end if
       istack_diff_coefs = 0
!
       end subroutine allocate_model_coefs
!
!  --------------------------------------------------------------------
!
      subroutine allocate_nod_model_coefs(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( ak_sgs_nod(numnod,num_sgs_coefs) )
      allocate( ak_diff_nod(numnod,num_diff_kinds) )
!
      if (num_sgs_kinds .gt. 0) ak_sgs_nod =  1.0d0 
      if (num_diff_kinds .gt. 0) ak_diff_nod = 1.0d0
!
      end subroutine allocate_nod_model_coefs
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
       subroutine deallocate_model_coefs
!
!
       deallocate( ncomp_sgs_coefs )
       deallocate( istack_sgs_coefs )
       deallocate( iflag_sgs_coefs )
       deallocate( ncomp_diff_coefs )
       deallocate( istack_diff_coefs )
       deallocate( iflag_diff_coefs )
       deallocate( ak_sgs )
       deallocate( ak_diff )
!
       end subroutine deallocate_model_coefs
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_nod_model_coefs
!
!
       deallocate( ak_sgs_nod )
       deallocate( ak_diff_nod )
!
       end subroutine deallocate_nod_model_coefs
!
!  --------------------------------------------------------------------
!
      end module m_SGS_model_coefs
