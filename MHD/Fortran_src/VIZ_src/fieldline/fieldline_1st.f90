!fieldline_1st.f90
!      module fieldline_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_fline
!      subroutine visualize_fline(istep_fline, ele_4_nod)
!
!      subroutine field_line_init_1st
!      subroutine field_line_main_1st(istep_psf, ele_4_nod)
!
      module fieldline_1st
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_fline
!
      use m_control_data_flines
      use m_control_params_4_fline
!
!
      num_fline = num_fline_ctl
      if (num_fline .gt. 0) call field_line_init_1st
!
      end subroutine init_visualize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_fline(istep_fline, ele_4_nod)
!
      use m_control_params_4_fline
      use t_next_node_ele_4_node
!
      integer(kind = kint), intent(in) :: istep_fline
      type(element_around_node), intent(in) :: ele_4_nod
!
!
      if (num_fline.gt.0 .and. istep_fline .gt. 0) then
        call field_line_main_1st(istep_fline, ele_4_nod)
      end if
!
      end subroutine visualize_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine field_line_init_1st
!
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
!
      use fieldline
!
!
      call field_line_init(node1, ele1, ele_grp1, sf_grp1, nod_fld1)
!
      end subroutine field_line_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_1st(istep_psf, ele_4_nod)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use t_next_node_ele_4_node
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf
      type(element_around_node), intent(in) :: ele_4_nod
!
!
      call field_line_main(istep_psf, node1, ele1, surf1, ele_grp1,     &
     &                     ele_4_nod, nod_fld1, nod_comm)
!
      end subroutine field_line_main_1st
!
!  ---------------------------------------------------------------------
!
      end module fieldline_1st
