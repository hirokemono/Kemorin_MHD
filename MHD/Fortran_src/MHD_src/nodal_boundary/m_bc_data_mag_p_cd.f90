!
!     module m_bc_data_mag_p_cd
!.......................................................................
!
!      Written by Kemorin
!
!      subroutine allocate_bc_mag_p_cd_4_ele
!
      module m_bc_data_mag_p_cd
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_index_ibc_mag_pc
      integer (kind=kint) :: num_index_ibc2_mag_pc
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ele_bc_mag_pc_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: nod_bc_mag_pc_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ele_bc2_mag_pc_id
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: nod_bc2_mag_pc_id
!
!
      integer (kind=kint) :: ibc_mag_pc_end
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pc_shape
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pc_stack
      integer (kind=kint), dimension(:), allocatable                    &
     &       :: ibc_mag_pc_stack_smp
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_mag_p_cd_4_ele
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_machine_parameter
!
        allocate ( ele_bc_mag_pc_id(num_index_ibc_mag_pc) )
        allocate ( nod_bc_mag_pc_id(num_index_ibc_mag_pc) )
!
        allocate ( ele_bc2_mag_pc_id(num_index_ibc2_mag_pc) )
        allocate ( nod_bc2_mag_pc_id(num_index_ibc2_mag_pc) )
!
        allocate ( ibc_mag_pc_stack(num_t_linear) )
        allocate ( ibc_mag_pc_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_mag_pc_shape(num_t_linear) )
!
        if ( num_index_ibc_mag_pc.gt.0) then
         ele_bc_mag_pc_id = 0
         nod_bc_mag_pc_id = 0
         ibc_mag_pc_stack = 0
         ibc_mag_pc_shape = 0
         ibc_mag_pc_stack_smp = 0
        end if
!
        if ( num_index_ibc2_mag_pc.gt.0) then
         ele_bc2_mag_pc_id = 0
         nod_bc2_mag_pc_id = 0
        end if
!
       end subroutine allocate_bc_mag_p_cd_4_ele
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_mag_p_cd
