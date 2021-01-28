(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ground

let serve in_ out =
  let rpc = Debug_rpc.create ~in_ ~out () in
  let cancel = ref (fun () -> ()) in
  Lwt.async (fun () ->
      ( try%lwt
          Log.debug (fun m -> m "state_uninitialized");
          let%lwt init_args, capabilities = State_uninitialized.run rpc in
          Log.debug (fun m -> m "state_initialized");
          let%lwt launch_args, dbg =
            State_initialized.run ~init_args ~capabilities rpc
          in
          State_debug.run ~launch_args ~dbg rpc;%lwt
          fst (Lwt.task ())
        with Exit -> Lwt.return_unit );%lwt
      Log.debug (fun m -> m "state_end");
      !cancel ();
      Lwt.return_unit);
  let loop = Debug_rpc.start rpc in
  (cancel := fun () -> Lwt.cancel loop);
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
  Log.debug (fun m -> m "loop end");
  Lwt.return ()
