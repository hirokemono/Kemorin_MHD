!
!     module m_bc_data_mag_p_ins
!.......................................................................
!
!      Written by Kemorin
!
!      subroutine allocate_bc_mag_p_ins_4_ele
!
      module m_bc_data_mag_p_ins
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: num_index_ibc_mag_pi
      integer (kind=kint) :: num_index_ibc2_mag_pi
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ele_bc_mag_pi_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: nod_bc_mag_pi_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ele_bc2_mag_pi_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: nod_bc2_mag_pi_id
! 
!
      integer (kind=kint) :: ibc_mag_pi_end
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pi_shape
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pi_stack
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pi_stack_smp
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_mag_p_ins_4_ele
!
      use m_geometry_constants
      use m_machine_parameter
!
        allocate ( ele_bc_mag_pi_id(num_index_ibc_mag_pi) )
        allocate ( nod_bc_mag_pi_id(num_index_ibc_mag_pi) )
!
        allocate ( ele_bc2_mag_pi_id(num_index_ibc2_mag_pi) )
        allocate ( nod_bc2_mag_pi_id(num_index_ibc2_mag_pi) )
!
        allocate ( ibc_mag_pi_stack(num_t_linear) )
        allocate ( ibc_mag_pi_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_mag_pi_shape(num_t_linear) )
!
        if ( num_index_ibc_mag_pi.gt.0) then
         ele_bc_mag_pi_id = 0
         nod_bc_mag_pi_id = 0
         ibc_mag_pi_stack = 0
         ibc_mag_pi_shape = 0
         ibc_mag_pi_stack_smp = 0
        end if
!
        if ( num_index_ibc2_mag_pi.gt.0) then
         ele_bc2_mag_pi_id = 0
         nod_bc2_mag_pi_id = 0
        end if
!
!
       end subroutine allocate_bc_mag_p_ins_4_ele
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_mag_p_ins
